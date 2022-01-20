#!/bin/bash
bin/Compiler "Tests/simpleIf.imp" "TestsResults/simpleIf.mr"
bin/interpreter "TestsResults/simpleIf.mr" > res

if [[ `egrep '> -?[0-9]+' res` == '> 1' ]]; then
    echo 'Plik dziala poprawnie'
else
    echo 'Plik nie dziala poprawnie'
fi