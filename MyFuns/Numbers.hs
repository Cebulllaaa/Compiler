{-# LANGUAGE ViewPatterns #-}
module MyFuns.Numbers (
  getExpression,
  generateWhile,
  getIdAddr
) where

import MyFuns.SimpleLanguage
import Gramma.Abs
import Data.Text as T (unpack)
import Data.List (genericReplicate, genericLength)
import MyFuns.Flow (genCond)
import MyFuns.Values 


generateWhile :: (Integer -> [OpCode]) -> [OpCode] -> [OpCode]
generateWhile codeC codeB =
  codeC' ++ codeB ++ [JUMP (CodePos (negate (lenC + lenB)))]
    where
      codeC' = codeC (lenB + 1)
      lenC = genericLength codeC'
      lenB = genericLength codeB

-- | GIVES VALUE IN REGISTER A
getExpression :: SymbolTable -> Expression -> [OpCode]
getExpression st (ValueExpr x) = getValue st C x ++ [SWAP C]
getExpression _ (Plus (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x + valueOf y) ++ [SWAP C]
getExpression _ (Minus (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x - valueOf y) ++ [SWAP C]
getExpression _ (Times (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x * valueOf y) ++ [SWAP C]
getExpression _ (Div (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x `quot` valueOf y) ++ [SWAP C]
getExpression _ (Mod (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x `rem` valueOf y) ++ [SWAP C]
getExpression st (Plus x y) =
  getValue st C x ++ getValue st D y ++ [SWAP C, ADD D]
getExpression st (Minus x y) =
  getValue st C x ++ getValue st D y ++ [SWAP C, SUB D]


getExpression st (Times x y) =
  getValue st C x ++ getValue st D y ++
  [RESET G, DEC G, RESET H, INC H, RESET E] ++
  [RESET A, ADD C, JPOS 7, RESET A, SUB C, SWAP C, RESET A, SUB D, SWAP D]++
  generateWhile (\skip -> [RESET A, ADD C, JZERO (CodePos (skip + 1))])
    [RESET A, ADD C, SHIFT G, SHIFT H, SUB C, JZERO 4,
     SWAP E, ADD D, SWAP E,
     SWAP C, SHIFT G, SWAP C, SWAP D, SHIFT H, SWAP D]
  ++ [SWAP E]
getExpression st (Div x y) =
  getValue st C x ++ getValue st D y ++ 
  [RESET G, RESET H, INC H, RESET E] ++
  [RESET A, ADD D, JZERO (CodePos (7 + 9 + 9 + 21 + 8 + 1))] ++
  [JPOS 7, RESET A, SUB D ,SWAP D, RESET A, SUB C, SWAP C] ++
  [RESET A, SWAP F, RESET A, ADD C, JPOS 4, INC F, RESET A, SUB C, SWAP C] ++
  generateWhile (\skip ->
    [RESET A, ADD C, SUB D, JNEG (CodePos (skip + 1))])
    [SWAP D, SHIFT H, SWAP D, INC G] ++
  generateWhile (\skip ->
    [SWAP G, JZERO (CodePos (skip + 1))])
    [SWAP G, DEC G, SWAP E, SHIFT H, SWAP E,
    SWAP D, DEC H, DEC H, SHIFT H, INC H, INC H, SWAP D, 
    RESET A, ADD C, SUB D, JNEG 3, SWAP C, INC E] ++ 
  [SWAP F, JZERO 7, RESET A, SUB E, SWAP E, SWAP C, JZERO 2, DEC E]
  ++ [SWAP E]

getExpression st (Mod x y) =
  getValue st C x ++ getValue st D y ++
  [RESET G, DEC G, RESET H, INC H, RESET E] ++
  [RESET A, ADD D, JZERO (CodePos (9+14+5+1))] ++
  generateWhile (\skip -> [RESET A, ADD C, SUB D, JNEG (CodePos (skip + 1))])
    [SWAP D, SHIFT H, SWAP D, INC E] ++
  generateWhile (\skip ->
    [RESET A, ADD E, JPOS (CodePos 2), JUMP (CodePos (skip +1))])
    [RESET A, ADD C, SUB D, JNEG 2, SWAP C, SWAP D, SHIFT G, SWAP D, DEC E] ++
  [RESET A, ADD C, SUB D, JNEG 3, JUMP 3, RESET C, SWAP C]

