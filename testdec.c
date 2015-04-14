#include <stdio.h>
#include <malloc.h>
#include <bsreader.h>

int main( int argc, const char* argv[] )
{
  FILE* file;
  void* buffer;
  size_t size;
  void* bs;
  const char* piece;
  
  if ( argc != 2 )
  {
    printf( "Usage: testdec <input.bs>\n" );
    return 1;
  }
  
  file = fopen( argv[ 1 ], "rb" );
  fseek( file, 0, SEEK_END );
  size = ftell( file );
  fseek( file, 0, SEEK_SET );
  
  buffer = malloc( size );
  fread( buffer, 1, size, file );
  fclose( file );
  
  bs = bsnew( buffer );
  
  for ( ;; )
  {
    piece = bsread( NULL, bs, &size );
    
    if ( !piece )
    {
      break;
    }
    
    printf( "%s", piece );
  }
  
  free( bs );
  return 0;
}
