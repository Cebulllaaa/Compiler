module MyFuns.Flow (
  genCond
) where
import MyFuns.SimpleLanguage
import Gramma.Abs
import MyFuns.Values 
import Debug.Trace
import Data.List (genericLength)

minus :: SymbolTable -> Value -> Value -> [OpCode]
minus st x y =
  getX ++ getY ++ [SWAP B, SUB C]
    where
      getX = getValue st B x
      getY = getValue st C y

genCond :: SymbolTable -> Condition -> Integer -> [OpCode]
genCond st (Neq x y) skip =
  minus st x y ++ [JZERO (CodePos (skip + 1))]
genCond st (Eq x y) skip =
  minus st x y ++ [JZERO 2, JUMP (CodePos (skip + 1))]
genCond st (Le x y) skip =
  minus st x y ++ [JNEG 2, JUMP (CodePos (skip + 1))]
genCond st (Ge x y) skip =
  minus st x y ++ [JPOS 2, JUMP (CodePos (skip + 1))]
genCond st (Leq x y) skip =
  minus st x y ++ [JPOS (CodePos (skip + 1))]
genCond st (Geq x y) skip =
  minus st x y ++ [JNEG (CodePos (skip + 1))]
