#!/bin/bash
PASSED=0
TOTAL=0
for SRC in `ls imp/*.imp`; do
  IN=in/`basename -s .imp $SRC`.in
  ACT=actual/`basename -s .imp $SRC`.out
  EXP=expected/`basename -s .imp $SRC`.out
  DST=mr/`basename -s .imp $SRC`.mr
  bin/Compiler $SRC $DST
  if ! [ -f $IN ]; then
    touch $IN
  fi
  if ! [ -f $EXP ]; then
    echo '> 1' >$EXP
  fi
  bin/interpreter $DST <$IN | grep -E -o '> -?[0-9]+' >$ACT
  diff $ACT $EXP
  if [ $? -eq 0 ]; then
    PASSED=$((PASSED + 1))
  else
    echo "Test $SRC fails"
  fi
  let TOTAL=$((TOTAL + 1))
done
if [ $PASSED -lt $TOTAL ]; then
  echo "Some tests have failed ($PASSED/$TOTAL passed)"
else
  echo "All tests have passed ($PASSED/$TOTAL passed)"
fi
