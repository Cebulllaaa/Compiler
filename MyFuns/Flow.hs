module MyFuns.Flow (
  genCond
) where
import MyFuns.SimpleLanguage
import Gramma.Abs
import MyFuns.Numbers (SymbolTable, valueOf, getValue)
import Debug.Trace
import Data.List (genericLength)

test :: SymbolTable -> Value -> Value -> (Integer -> [OpCode]) -> (Integer -> [OpCode]) -> (CodePos -> OpCode) -> [OpCode]
test st x y s f jump =
  getX ++ getY ++ [SWAP B, SUB C, jump (CodePos fail)] ++
  f prolog ++ [JUMP (CodePos success)] ++ s (prolog + fail)
    where
      getX = getValue st B x
      getY = getValue st C y
      prolog = genericLength getX + genericLength getY + 3
      fail = genericLength (f prolog) + 1
      success = genericLength (s (prolog + fail))

genCond :: SymbolTable -> Condition -> (Integer -> [OpCode]) -> (Integer -> [OpCode]) -> [OpCode]
genCond st (Eq x y) s f =
  test st x y s f JZERO
genCond st (Neq x y) s f =
  test st x y f s JZERO
genCond st (Le x y) s f =
  test st x y f s JNEG
genCond st (Ge x y) s f =
  test st x y f s JPOS
genCond st (Leq x y) s f =
  test st x y s f JPOS
genCond st (Geq x y) s f =
  test st x y s f JNEG
