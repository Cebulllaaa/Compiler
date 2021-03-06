/*
 * Kod maszyny wirtualnej do projektu z JFTT2021
 *
 * Autor: Maciek Gębala
 * http://ki.pwr.edu.pl/gebala/
 * 2021-11-12
*/
#include <iostream>

#include <utility>
#include <vector>

#include "colors.hh"

using namespace std;

extern void run_parser( vector< pair<int,int> > & program, FILE * data );
extern void run_machine( vector< pair<int,int> > & program );

int main( int argc, char const * argv[] )
{
  vector< pair<int,int> > program;
  FILE * data;

  if( argc!=2 )
  {
  rr << cRed << "Sposób użycia programu: interpreter kod" << cReset << endl;
  turn -1;
  }

  data = fopen( argv[1], "r" );
  if( !data )
  {
  rr << cRed << "Błąd: Nie można otworzyć pliku " << argv[1] << cReset << endl;
  turn -1;
  }

  run_parser( program, data );

  fclose( data );

  run_machine( program );

  return 0;
}
