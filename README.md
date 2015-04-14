# bstree

**bstree** is Lua source code obfuscator. It analyzes Lua files in source code and outputs an encoder and a decoder.

BS stands for Bit Stream or Bull Shit, depending on your mood.

## Usage

    $ lua bstree.lua <input.lua>+

This command will analyze all given Lua files and output two files:

1. `bsenc.lua`: Lua application that obfuscates the given source code
2. `bstree.h`: header file with the decode tree, used by `bsreader.c`

## bsenc.lua

    $ lua bsenc.lua <input.lua> <output.bs>

Obfuscates the given Lua file in source code format, producing `output.bs`. Use `output.bs` in your applications.

## bsreader.c

To read the obfuscated files in your application, use `bsreader.c`. It requires `bstree.h`, created by running `bstree.lua`. `bsreader.h` provides two functions:

* `void* bsnew( void* data );`: creates a structure used by `bsread`.
* `const char* bsread( lua_State* L, void* data, size_t* size );`: a `lua_Reader` function that takes the structure created by `bsnew` and produces Lua source code. `bsread` is meant to be used with `lua_load`.

See `testdec.c` for a decoder example.

## Details

`bstree.lua` analyzes Lua source code and creates a Huffman tree. Identifiers, numbers and strings are all encoded as literals, with the characters following the encoding, and the remaining Lua keywords and symbols are encoded as themselves. `self`, despiting begin an identifier, is encoded as a keyword. A special keyword, `eof` is also added to the tree to mark the end of the encoded data.

`bstree.lua` then outputs `bsenc.lua` and `bstree.h`, both with the Huffman tree embeded. They must be used in pairs, i.e. files encoded by `bsenc.lua` can only be decoded by `bstree.h` that was created by the same `bstree.lua` run as `bsenc.lua`.

The `bsread` function uses a predefined buffer during the decoding process. Its size if 512 bytes by default, make sure you edit `bsread.c` and change `MAX_LITERAL_SIZE` according to your needs. The default size should be enough for most applications, but if you have large string literals it won't be able to decode the entire literal and bad things will happen.

The encoded files are easily decoded if `bstree.h` is available, or if it's recreated via reverse engineering. This utility should **not** be used if you require **absolute** secrecy about your Lua source code.

`bstree.lua` and the generated `bsenc.lua` files uses [Penlight](https://github.com/stevedonovan/Penlight)'s `lexer.lua`, modified to include the integer division symbol `//` from Lua version 5.3. `lexer.lua` is included verbatim where it's used because I hate LUA_PATH.

## License

`bstree.lua` is released under the zlib/libpng license. `bsenc.lua` and `bstree.h` are not subject to this license, and can be used according to your own terms. `bsreader.c` and `bsreader.h` are in the public domain.
