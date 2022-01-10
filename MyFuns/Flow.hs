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
  getX ++ getY ++ [SWAP B, SUB C, jump (CodePos (success + 1))] ++
  f' ++ [JUMP (CodePos end)] ++ s'
    where
      getX = getValue st B x
      getY = getValue st C y
      prolog = genericLength getX + genericLength getY + 3
      f' = f prolog
      success = genericLength f' + 1
      s' = s (prolog + success)
      end = genericLength s' + 1

genCond :: SymbolTable -> Condition -> (Integer -> [OpCode]) -> (Integer -> [OpCode]) -> [OpCode]
genCond st (Eq x y) s f =
  test st x y s f JZERO
genCond st (Neq x y) s f =
  test st x y f s JZERO
genCond st (Le x y) s f =
  test st x y s f JNEG
genCond st (Ge x y) s f =
  test st x y s f JPOS
genCond st (Leq x y) s f =
  test st x y f s JPOS
genCond st (Geq x y) s f =
  test st x y f s JNEG
