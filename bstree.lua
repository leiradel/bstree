local pl_lexer = [===[
--- Lexical scanner for creating a sequence of tokens from text.
-- `lexer.scan(s)` returns an iterator over all tokens found in the
-- string `s`. This iterator returns two values, a token type string
-- (such as 'string' for quoted string, 'iden' for identifier) and the value of the
-- token.
--
-- Versions specialized for Lua and C are available; these also handle block comments
-- and classify keywords as 'keyword' tokens. For example:
--
--    > s = 'for i=1,n do'
--    > for t,v in lexer.lua(s)  do print(t,v) end
--    keyword for
--    iden    i
--    =       =
--    number  1
--    ,       ,
--    iden    n
--    keyword do
--
-- See the Guide for further @{06-data.md.Lexical_Scanning|discussion}
-- @module pl.lexer

local yield,wrap = coroutine.yield,coroutine.wrap
local strfind = string.find
local strsub = string.sub
local append = table.insert

local function assert_arg(idx,val,tp)
    if type(val) ~= tp then
        error("argument "..idx.." must be "..tp, 2)
    end
end

local lexer = {}

local NUMBER1 = '^[%+%-]?%d+%.?%d*[eE][%+%-]?%d+'
local NUMBER2 = '^[%+%-]?%d+%.?%d*'
local NUMBER3 = '^0x[%da-fA-F]+'
local NUMBER4 = '^%d+%.?%d*[eE][%+%-]?%d+'
local NUMBER5 = '^%d+%.?%d*'
local IDEN = '^[%a_][%w_]*'
local WSPACE = '^%s+'
local STRING0 = [[^(['\"]).-\\%1]]
local STRING1 = [[^(['\"]).-[^\]%1]]
local STRING3 = "^((['\"])%2)" -- empty string
local PREPRO = '^#.-[^\\]\n'

local plain_matches,lua_matches,cpp_matches,lua_keyword,cpp_keyword

local function tdump(tok)
    return yield(tok,tok)
end

local function ndump(tok,options)
    if options and options.number then
        tok = tonumber(tok)
    end
    return yield("number",tok)
end

-- regular strings, single or double quotes; usually we want them
-- without the quotes
local function sdump(tok,options)
    if options and options.string then
        tok = tok:sub(2,-2)
    end
    return yield("string",tok)
end

-- long Lua strings need extra work to get rid of the quotes
local function sdump_l(tok,options,findres)
    if options and options.string then
        local quotelen = 3
        if findres[3] then
            quotelen = quotelen + findres[3]:len()
        end
        tok = tok:sub(quotelen,-1 * quotelen)
    end
    return yield("string",tok)
end

local function chdump(tok,options)
    if options and options.string then
        tok = tok:sub(2,-2)
    end
    return yield("char",tok)
end

local function cdump(tok)
    return yield('comment',tok)
end

local function wsdump (tok)
    return yield("space",tok)
end

local function pdump (tok)
    return yield('prepro',tok)
end

local function plain_vdump(tok)
    return yield("iden",tok)
end

local function lua_vdump(tok)
    if lua_keyword[tok] then
        return yield("keyword",tok)
    else
        return yield("iden",tok)
    end
end

local function cpp_vdump(tok)
    if cpp_keyword[tok] then
        return yield("keyword",tok)
    else
        return yield("iden",tok)
    end
end

--- create a plain token iterator from a string or file-like object.
-- @string s the string
-- @tab matches an optional match table (set of pattern-action pairs)
-- @tab[opt] filter a table of token types to exclude, by default `{space=true}`
-- @tab[opt] options a table of options; by default, `{number=true,string=true}`,
-- which means convert numbers and strip string quotes.
function lexer.scan (s,matches,filter,options)
    --assert_arg(1,s,'string')
    local file = type(s) ~= 'string' and s
    filter = filter or {space=true}
    options = options or {number=true,string=true}
    if filter then
        if filter.space then filter[wsdump] = true end
        if filter.comments then
            filter[cdump] = true
        end
    end
    if not matches then
        if not plain_matches then
            plain_matches = {
                {WSPACE,wsdump},
                {NUMBER3,ndump},
                {IDEN,plain_vdump},
                {NUMBER1,ndump},
                {NUMBER2,ndump},
                {STRING3,sdump},
                {STRING0,sdump},
                {STRING1,sdump},
                {'^.',tdump}
            }
        end
        matches = plain_matches
    end
    local function lex ()
        if type(s)=='string' and s=='' then return end
        local findres,i1,i2,idx,res1,res2,tok,pat,fun,capt
        local line = 1
        if file then s = file:read()..'\n' end
        local sz = #s
        local idx = 1
        --print('sz',sz)
        while true do
            for _,m in ipairs(matches) do
                pat = m[1]
                fun = m[2]
                findres = { strfind(s,pat,idx) }
                i1 = findres[1]
                i2 = findres[2]
                if i1 then
                    tok = strsub(s,i1,i2)
                    idx = i2 + 1
                    if not (filter and filter[fun]) then
                        lexer.finished = idx > sz
                        res1,res2 = fun(tok,options,findres)
                    end
                    if res1 then
                        local tp = type(res1)
                        -- insert a token list
                        if tp=='table' then
                            yield('','')
                            for _,t in ipairs(res1) do
                                yield(t[1],t[2])
                            end
                        elseif tp == 'string' then -- or search up to some special pattern
                            i1,i2 = strfind(s,res1,idx)
                            if i1 then
                                tok = strsub(s,i1,i2)
                                idx = i2 + 1
                                yield('',tok)
                            else
                                yield('','')
                                idx = sz + 1
                            end
                            --if idx > sz then return end
                        else
                            yield(line,idx)
                        end
                    end
                    if idx > sz then
                        if file then
                            --repeat -- next non-empty line
                                line = line + 1
                                s = file:read()
                                if not s then return end
                            --until not s:match '^%s*$'
                            s = s .. '\n'
                            idx ,sz = 1,#s
                            break
                        else
                            return
                        end
                    else break end
                end
            end
        end
    end
    return wrap(lex)
end

local function isstring (s)
    return type(s) == 'string'
end

--- insert tokens into a stream.
-- @param tok a token stream
-- @param a1 a string is the type, a table is a token list and
-- a function is assumed to be a token-like iterator (returns type & value)
-- @string a2 a string is the value
function lexer.insert (tok,a1,a2)
    if not a1 then return end
    local ts
    if isstring(a1) and isstring(a2) then
        ts = {{a1,a2}}
    elseif type(a1) == 'function' then
        ts = {}
        for t,v in a1() do
            append(ts,{t,v})
        end
    else
        ts = a1
    end
    tok(ts)
end

--- get everything in a stream upto a newline.
-- @param tok a token stream
-- @return a string
function lexer.getline (tok)
    local t,v = tok('.-\n')
    return v
end

--- get current line number.
-- Only available if the input source is a file-like object.
-- @param tok a token stream
-- @return the line number and current column
function lexer.lineno (tok)
    return tok(0)
end

--- get the rest of the stream.
-- @param tok a token stream
-- @return a string
function lexer.getrest (tok)
    local t,v = tok('.+')
    return v
end

--- get the Lua keywords as a set-like table.
-- So `res["and"]` etc would be `true`.
-- @return a table
function lexer.get_keywords ()
    if not lua_keyword then
        lua_keyword = {
            ["and"] = true, ["break"] = true,  ["do"] = true,
            ["else"] = true, ["elseif"] = true, ["end"] = true,
            ["false"] = true, ["for"] = true, ["function"] = true,
            ["if"] = true, ["in"] = true,  ["local"] = true, ["nil"] = true,
            ["not"] = true, ["or"] = true, ["repeat"] = true,
            ["return"] = true, ["then"] = true, ["true"] = true,
            ["until"] = true,  ["while"] = true
        }
    end
    return lua_keyword
end

--- create a Lua token iterator from a string or file-like object.
-- Will return the token type and value.
-- @string s the string
-- @tab[opt] filter a table of token types to exclude, by default `{space=true,comments=true}`
-- @tab[opt] options a table of options; by default, `{number=true,string=true}`,
-- which means convert numbers and strip string quotes.
function lexer.lua(s,filter,options)
    filter = filter or {space=true,comments=true}
    lexer.get_keywords()
    if not lua_matches then
        lua_matches = {
            {WSPACE,wsdump},
            {NUMBER3,ndump},
            {IDEN,lua_vdump},
            {NUMBER4,ndump},
            {NUMBER5,ndump},
            {STRING3,sdump},
            {STRING0,sdump},
            {STRING1,sdump},
            {'^%-%-%[(=*)%[.-%]%1%]',cdump},
            {'^%-%-.-\n',cdump},
            {'^%[(=*)%[.-%]%1%]',sdump_l},
            {'^==',tdump},
            {'^~=',tdump},
            {'^<=',tdump},
            {'^>=',tdump},
            {'^%.%.%.',tdump},
            {'^%.%.',tdump},
            {'^//',tdump},
            {'^.',tdump}
        }
    end
    return lexer.scan(s,lua_matches,filter,options)
end

--- create a C/C++ token iterator from a string or file-like object.
-- Will return the token type type and value.
-- @string s the string
-- @tab[opt] filter a table of token types to exclude, by default `{space=true,comments=true}`
-- @tab[opt] options a table of options; by default, `{number=true,string=true}`,
-- which means convert numbers and strip string quotes.
function lexer.cpp(s,filter,options)
    filter = filter or {comments=true}
    if not cpp_keyword then
        cpp_keyword = {
            ["class"] = true, ["break"] = true,  ["do"] = true, ["sizeof"] = true,
            ["else"] = true, ["continue"] = true, ["struct"] = true,
            ["false"] = true, ["for"] = true, ["public"] = true, ["void"] = true,
            ["private"] = true, ["protected"] = true, ["goto"] = true,
            ["if"] = true, ["static"] = true,  ["const"] = true, ["typedef"] = true,
            ["enum"] = true, ["char"] = true, ["int"] = true, ["bool"] = true,
            ["long"] = true, ["float"] = true, ["true"] = true, ["delete"] = true,
            ["double"] = true,  ["while"] = true, ["new"] = true,
            ["namespace"] = true, ["try"] = true, ["catch"] = true,
            ["switch"] = true, ["case"] = true, ["extern"] = true,
            ["return"] = true,["default"] = true,['unsigned']  = true,['signed'] = true,
            ["union"] =  true, ["volatile"] = true, ["register"] = true,["short"] = true,
        }
    end
    if not cpp_matches then
        cpp_matches = {
            {WSPACE,wsdump},
            {PREPRO,pdump},
            {NUMBER3,ndump},
            {IDEN,cpp_vdump},
            {NUMBER4,ndump},
            {NUMBER5,ndump},
            {STRING3,sdump},
            {STRING1,chdump},
            {'^//.-\n',cdump},
            {'^/%*.-%*/',cdump},
            {'^==',tdump},
            {'^!=',tdump},
            {'^<=',tdump},
            {'^>=',tdump},
            {'^->',tdump},
            {'^&&',tdump},
            {'^||',tdump},
            {'^%+%+',tdump},
            {'^%-%-',tdump},
            {'^%+=',tdump},
            {'^%-=',tdump},
            {'^%*=',tdump},
            {'^/=',tdump},
            {'^|=',tdump},
            {'^%^=',tdump},
            {'^::',tdump},
            {'^.',tdump}
        }
    end
    return lexer.scan(s,cpp_matches,filter,options)
end

--- get a list of parameters separated by a delimiter from a stream.
-- @param tok the token stream
-- @string[opt=')'] endtoken end of list. Can be '\n'
-- @string[opt=','] delim separator
-- @return a list of token lists.
function lexer.get_separated_list(tok,endtoken,delim)
    endtoken = endtoken or ')'
    delim = delim or ','
    local parm_values = {}
    local level = 1 -- used to count ( and )
    local tl = {}
    local function tappend (tl,t,val)
        val = val or t
        append(tl,{t,val})
    end
    local is_end
    if endtoken == '\n' then
        is_end = function(t,val)
            return t == 'space' and val:find '\n'
        end
    else
        is_end = function (t)
            return t == endtoken
        end
    end
    local token,value
    while true do
        token,value=tok()
        if not token then return nil,'EOS' end -- end of stream is an error!
        if is_end(token,value) and level == 1 then
            append(parm_values,tl)
            break
        elseif token == '(' then
            level = level + 1
            tappend(tl,'(')
        elseif token == ')' then
            level = level - 1
            if level == 0 then -- finished with parm list
                append(parm_values,tl)
                break
            else
                tappend(tl,')')
            end
        elseif token == delim and level == 1 then
            append(parm_values,tl) -- a new parm
            tl = {}
        else
            tappend(tl,token,value)
        end
    end
    return parm_values,{token,value}
end

--- get the next non-space token from the stream.
-- @param tok the token stream.
function lexer.skipws (tok)
    local t,v = tok()
    while t == 'space' do
        t,v = tok()
    end
    return t,v
end

local skipws = lexer.skipws

--- get the next token, which must be of the expected type.
-- Throws an error if this type does not match!
-- @param tok the token stream
-- @string expected_type the token type
-- @bool no_skip_ws whether we should skip whitespace
function lexer.expecting (tok,expected_type,no_skip_ws)
    assert_arg(1,tok,'function')
    assert_arg(2,expected_type,'string')
    local t,v
    if no_skip_ws then
        t,v = tok()
    else
        t,v = skipws(tok)
    end
    if t ~= expected_type then error ("expecting "..expected_type,2) end
    return v
end

return lexer
]===]

local lexer = load( pl_lexer, 'lexer.lua' )()

local function errorout( ... )
  local args = { ... }
  local format = args[ 1 ]
  table.remove( args, 1 )
  io.stderr:write( string.format( format, args ), '\n' )
  os.exit( 1 )
end

local function writeenc( enctab )
  local file, err = io.open( 'bsenc.lua', 'w' )
  
  if not file then
    errorout( '%s\n', err )
  end
  
  file:write( 'local pl_lexer = [===[\n' )
  file:write( pl_lexer )
  file:write( ']===]\n\n' )
  file:write( 'local lexer = load( pl_lexer, \'lexer.lua\' )()\n\n' )
  file:write( 'local enctab = {\n' )
  file:write( table.concat( enctab ) )
  file:write( '}\n\n' )
  file:write[===[
local function errorout( ... )
  local args = { ... }
  local format = args[ 1 ]
  table.remove( args, 1 )
  io.stderr:write( string.format( format, args ), '\n' )
  os.exit( 1 )
end

local function addbits( encoded, bits )
  for i = 1, #bits do
    encoded[ #encoded + 1 ] = bits:sub( i, i ) + 0
  end
end

local function addliteral( encoded, str )
  if #encoded % 8 == 0 then
    addbits( encoded, '1' )
  end
  
  for i = 1, #str do
    local k = str:byte( i, i )
    
    for j = 7, 0, -1 do
      if bit32.band( k, bit32.lshift( 1, j ) ) ~= 0 then
        addbits( encoded, '1' )
      else
        addbits( encoded, '0' )
      end
    end
  end
  
  addbits( encoded, '00000000' )
end

local function bitstobyte( encoded, i )
  local bit = 128
  local byte = 0
  
  for j = 0, 7 do
    byte = byte + encoded[ i + j ] * bit
    bit = bit / 2
  end
  
  return string.char( byte )
end

local function encode( source )
  local encoded = {}
  
  for token, lexeme in lexer.lua( source ) do
    local bits
    
    if token == 'keyword' then
      bits = enctab[ lexeme ]
    elseif token == 'iden' or token == 'number' or token == 'string' then
      bits = enctab.literal
    else
      bits = enctab[ token ]
    end
    
    addbits( encoded, bits )
    
    if token == 'iden' then
      addliteral( encoded, lexeme )
    elseif token == 'number' then
      addliteral( encoded, tostring( lexeme ) )
    elseif token == 'string' then
      addliteral( encoded, string.format( '%q', lexeme ) )
    end
  end
  
  addbits( encoded, enctab.eof )
  
  while #encoded %8 ~= 0 do
    addbits( encoded, '0' )
  end
  
  local s = {}
  
  for i = 1, #encoded, 8 do
    s[ #s + 1 ] = bitstobyte( encoded, i )
  end
  
  return table.concat( s )
end

local function main( args )
  if #args ~= 2 then
    io.write( 'Usage: lua bsenc.lua <input.lua> <output.lua>\n' )
    return 0
  end
  
  local file, err = io.open( args[ 1 ] )
  
  if not file then
    errorout( 'Error opening %s', args[ 1 ] )
  end
  
  local source = file:read( '*a' )
  file:close()
  
  if not source then
    errorout( 'Could not read from %s', args[ 1 ] )
  end
  
  local encoded = encode( source )
  
  file, err = io.open( args[ 2 ], 'wb' )
  
  if not file then
    errorout( 'Error opening %s', args[ 2 ] )
  end
  
  file:write( encoded )
  file:close()
end

return main( arg )
  ]===]
  
  file:close()
end

local function writedec( dectab, map, root )
  local file, err = io.open( 'bstree.h', 'w' )
  
  if not file then
    errorout( '%s\n', err )
  end
  
  file:write( '#ifndef BSTREE_H\n' )
  file:write( '#define BSTREE_H\n\n' )
  
  file:write( 'typedef struct node_t node_t;\n\n' )
  file:write( 'struct node_t\n' )
  file:write( '{\n' )
  file:write( '  const node_t* left;\n' )
  file:write( '  const node_t* right;\n' )
  file:write( '  char          token;\n' )
  file:write( '};\n\n' )
  
  file:write( table.concat( dectab ) )
  
  file:write( '\n' )
  file:write( 'static struct{ const char* literal; size_t len; } tokens[] =\n' )
  file:write( '{\n' )
  
  for _, token in ipairs( map ) do
    if token ~= 'literal' and token ~= 'eof' then
      file:write( '  { "', token, '", ', #token, ' },\n' )
    else
      file:write( '  { NULL, 0 },\n' )
    end
  end
  
  file:write( '};\n\n' )
  
  file:write( '#define BS_ROOT    ( &', root, ' )\n' )
  file:write( '#define BS_LITERAL ', map.literal, '\n' )
  file:write( '#define BS_EOF     ', map.eof, '\n\n' )
  
  file:write( '#endif /* BSTREE_H */\n' )
  file:close()
end

local function buildtree( source )
  local tokens = {}
  
  for token, lexeme in lexer.lua( source ) do
    if token == 'keyword' then
      tokens[ lexeme ] = ( tokens[ lexeme ] or 0 ) + 1
    elseif token == 'iden' and lexeme == 'self' then
      tokens.self = ( tokens.self or 0 ) + 1
    elseif token == 'iden' or token == 'number' or token == 'string' then
      tokens.literal = ( tokens.literal or 0 ) + 1
    else
      tokens[ token ] = ( tokens[ token ] or 0 ) + 1
    end
  end
  
  tokens[ 'and' ] = tokens[ 'and' ] or 0
  tokens[ 'break' ] = tokens[ 'break' ] or 0
  tokens[ 'do' ] = tokens[ 'do' ] or 0
  tokens[ 'else' ] = tokens[ 'else' ] or 0
  tokens[ 'elseif' ] = tokens[ 'elseif' ] or 0
  tokens[ 'end' ] = tokens[ 'end' ] or 0
  tokens[ 'false' ] = tokens[ 'false' ] or 0
  tokens[ 'for' ] = tokens[ 'for' ] or 0
  tokens[ 'function' ] = tokens[ 'function' ] or 0
  tokens[ 'goto' ] = tokens[ 'goto' ] or 0
  tokens[ 'if' ] = tokens[ 'if' ] or 0
  tokens[ 'in' ] = tokens[ 'in' ] or 0
  tokens[ 'local' ] = tokens[ 'local' ] or 0
  tokens[ 'nil' ] = tokens[ 'nil' ] or 0
  tokens[ 'not' ] = tokens[ 'not' ] or 0
  tokens[ 'or' ] = tokens[ 'or' ] or 0
  tokens[ 'repeat' ] = tokens[ 'repeat' ] or 0
  tokens[ 'return' ] = tokens[ 'return' ] or 0
  tokens[ 'then' ] = tokens[ 'then' ] or 0
  tokens[ 'true' ] = tokens[ 'true' ] or 0
  tokens[ 'until' ] = tokens[ 'until' ] or 0
  tokens[ 'while' ] = tokens[ 'while' ] or 0
  tokens[ '+' ] = tokens[ '+' ] or 0
  tokens[ '-' ] = tokens[ '-' ] or 0
  tokens[ '*' ] = tokens[ '*' ] or 0
  tokens[ '/' ] = tokens[ '/' ] or 0
  tokens[ '%' ] = tokens[ '%' ] or 0
  tokens[ '^' ] = tokens[ '^' ] or 0
  tokens[ '#' ] = tokens[ '#' ] or 0
  tokens[ '&' ] = tokens[ '&' ] or 0
  tokens[ '~' ] = tokens[ '~' ] or 0
  tokens[ '|' ] = tokens[ '|' ] or 0
  tokens[ '<<' ] = tokens[ '<<' ] or 0
  tokens[ '>>' ] = tokens[ '>>' ] or 0
  tokens[ '//' ] = tokens[ '//' ] or 0
  tokens[ '==' ] = tokens[ '==' ] or 0
  tokens[ '~=' ] = tokens[ '~=' ] or 0
  tokens[ '<=' ] = tokens[ '<=' ] or 0
  tokens[ '>=' ] = tokens[ '>=' ] or 0
  tokens[ '<' ] = tokens[ '<' ] or 0
  tokens[ '>' ] = tokens[ '>' ] or 0
  tokens[ '=' ] = tokens[ '=' ] or 0
  tokens[ '(' ] = tokens[ '(' ] or 0
  tokens[ ')' ] = tokens[ ')' ] or 0
  tokens[ '{' ] = tokens[ '{' ] or 0
  tokens[ '}' ] = tokens[ '}' ] or 0
  tokens[ '[' ] = tokens[ '[' ] or 0
  tokens[ ']' ] = tokens[ ']' ] or 0
  tokens[ '::' ] = tokens[ '::' ] or 0
  tokens[ ';' ] = tokens[ ';' ] or 0
  tokens[ ':' ] = tokens[ ':' ] or 0
  tokens[ ',' ] = tokens[ ',' ] or 0
  tokens[ '.' ] = tokens[ '.' ] or 0
  tokens[ '..' ] = tokens[ '..' ] or 0
  tokens[ '...' ] = tokens[ '...' ] or 0
  
  tokens.literal = tokens.literal or 0
  tokens.self = tokens.self or 0
  tokens.eof = 0
  
  local trees = {}
  
  for token, count in pairs( tokens ) do
    trees[ #trees + 1 ] = { token = token, freq = count }
  end
  
  while #trees ~= 1 do
    local t1, t2
    local i, freq
    
    for j, t in ipairs( trees ) do
      if not freq or t.freq < freq then
        t1 = t
        freq = t.freq
        i = j
      end
    end
    
    table.remove( trees, i )
    
    freq = nil
    
    for j, t in ipairs( trees ) do
      if not freq or t.freq < freq then
        t2 = t
        freq = t.freq
        i = j
      end
    end
    
    table.remove( trees, i )
    
    t = { left = t1, right = t2, freq = t1.freq + t2.freq }
    trees[ #trees + 1 ] = t
  end
  
  local enctab = {}
  
  local walk
  walk = function( tree, seq )
    if tree.left then
      walk( tree.left, seq .. '0' )
      walk( tree.right, seq .. '1' )
    else
      enctab[ #enctab + 1 ] = '  [ \'' .. tree.token .. '\' ] = \'' .. seq .. '\', -- ' .. tree.freq .. '\n'
    end
  end
  
  walk( trees[ 1 ], '' )
  writeenc( enctab )
  
  local map = {}
  local index = 0
  
  local id = function( t )
    return '_' .. tostring( t ):gsub( 'table:%s(.*)', '%1' )
  end
  
  local ndx = function( t )
    if not map[ t ] then
      map[ t ] = index
      index = index + 1
      map[ index ] = t
    end
    
    return map[ t ]
  end
  
  local dectab = {}
  
  local walk2
  walk2 = function( tree, seq )
    if tree.left then
      walk2( tree.left, seq .. '0' )
      walk2( tree.right, seq .. '1' )
      
      dectab[ #dectab + 1 ] = 'static const node_t ' .. id( tree ) .. ' = '
      dectab[ #dectab + 1 ] = '{ &' .. id( tree.left ) .. ', &' .. id( tree.right ) .. ', -1 };\n'
    else
      dectab[ #dectab + 1 ] = 'static const node_t ' .. id( tree ) .. ' = '
      dectab[ #dectab + 1 ] = '{ NULL, NULL, ' .. ndx( tree.token ) .. ' }; /* "' .. tree.token .. '" ' .. seq .. ' ' .. tokens[ tree.token ] .. ' */\n'
    end
  end
  
  
  walk2( trees[ 1 ], '' )
  writedec( dectab, map, id( trees[ 1 ] ) )
end

local function main( args )
  if #args < 1 then
    io.write( 'Usage: lua bstree.lua <input.lua>+\n' )
    return 0
  end
  
  local source = {}
  
  for i = 1, #args do
    local file, err = io.open( args[ i ] )
    
    if not file then
      errorout( 'Error reading from %s', args[ i ] )
    end
    
    local s = file:read( '*a' )
    file:close()
    
    if not s then
      errorout( 'Could not read from %s', args[ i ] )
    end
    
    source[ #source + 1 ] = s
  end
  
  buildtree( table.concat( source, '\n' ) )
end

return main( arg )
