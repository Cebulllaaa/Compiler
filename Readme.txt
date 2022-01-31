Autor : Bartosz Cybulski 
Język : Haskell
Wykorzystywane Programy:
    GHCI v8.6.5
    BNFC v2.9.3
    ALEX v3.2.4
    HAPPY v1.19.11
    cabal-install v2.4.0.0
Opis plikow :
    Main.hs - glowny plik ktory odpowiada za obsluge tablicy symboli, pamięci
        generowanie kodu assemblerowego, implementacje petli For
    Flow.hs - plik odpowiadajacy za obsluge warunkow, oraz okreslenia czy mozna 
        utworzyc szybszego fora 
    Numbers.hs - plik implementujacy petle while oraz dzialania na liczbach
    Values.hs - plik odpowiadjacy za alokacje zmiennych w pamieci 
    SimpleLanguage.hs - plik opisujacy kod assemblerowy
    Gramma.cf - plik opisujacy zadana gramatyke
Przykladowa konfiguracja :
    0) sudo apt-get update
    1) sudo apt install ghc 
    2) sudo apt install make
    3) sudo apt install cabal-install
    4) cabal update
    5) cabal install alex 
    6) cabal install happy 
    7) cabal install BNFC