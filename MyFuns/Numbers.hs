{-# LANGUAGE ViewPatterns #-}
module MyFuns.Numbers (
  VarInfo(..),
  SymbolTable,
  valueOf,
  getExpression,
  getValue,
  getIdAddr
) where

import MyFuns.SimpleLanguage
import Gramma.Abs
import Data.Text as T (unpack)
import Data.List (genericReplicate)
import Data.Map as M (Map, lookup)

data VarInfo
  = ScalarInfo {address :: Integer}
  | ArrayInfo {address :: Integer, begin :: Integer, end :: Integer}

type SymbolTable = M.Map Pidentifier VarInfo

valueOf :: Number -> Integer
valueOf (Number txt) = read $ unpack txt

maxSmallNumber :: Integer
maxSmallNumber = 3

-- | DO NOT USE REGISTER A
generateConstant :: Reg -> Integer -> [OpCode]
generateConstant A _ = error "register A is forbidden here"
generateConstant reg n
  | n >= 0 && n <= maxSmallNumber = [RESET reg] ++ genericReplicate n (INC reg)
  | n < 0 && n >= negate maxSmallNumber = [RESET reg] ++ genericReplicate n (DEC reg)
generateConstant reg n = [RESET reg, INC reg, RESET A] ++ gen n ++ [SWAP reg]
  where
    gen (-1) = [DEC A]
    gen 0 = []
    gen 1 = [INC A]
    gen k = gen q ++ [SHIFT reg] ++ gen r
      where
        (q, r) = quotRem k 2

getNumber :: Reg -> Number -> [OpCode]
getNumber reg x = generateConstant reg (valueOf x)

getValue :: SymbolTable -> Reg -> Value -> [OpCode]
getValue _ reg (NumValue x) = getNumber reg x
getValue st reg (IdValue x) = getIdValue st reg x

getIdValue :: SymbolTable -> Reg -> Identifier -> [OpCode]
getIdValue st reg ident = getIdAddr st reg ident ++ [LOAD reg, SWAP reg]

getIdAddr :: SymbolTable -> Reg -> Identifier -> [OpCode]
getIdAddr st reg (ScalarId pid@(Pidentifier txt)) =
  case M.lookup pid st of
    Nothing -> error $ "undeclared identifier: " ++ T.unpack txt
    Just (ScalarInfo addr) -> generateConstant reg addr
    Just _ -> error $ "array used as scalar: " ++ T.unpack txt
getIdAddr st reg (VarArrayId pid@(Pidentifier txt) pid') =
  case M.lookup pid st of
    Nothing -> error $ "undeclared identifier: " ++ T.unpack txt
    Just (ArrayInfo addr beg end) ->
      getIdValue st E (ScalarId pid') ++ generateConstant reg (addr - beg) ++ [SWAP reg, ADD E, LOAD A, SWAP reg]
    Just _ -> error $ "scalar used as array: " ++ T.unpack txt
getIdAddr st reg (ConstArrayId pid@(Pidentifier txt) (valueOf -> index)) =
  case M.lookup pid st of
    Nothing -> error $ "undeclared identifier: " ++ T.unpack txt
    Just (ArrayInfo addr beg end) ->
      if beg <= index && index <= end then
        generateConstant reg (addr - beg + index) ++ [SWAP reg, LOAD A, SWAP reg]
      else
        error $ "index out of range: " ++ T.unpack txt ++ "[" ++ show index ++ "]"
    Just _ -> error $ "scalar used as array: " ++ T.unpack txt

-- | GIVES VALUE IN REGISTER C
getExpression :: SymbolTable -> Expression -> [OpCode]
getExpression st (ValueExpr x) = getValue st C x
getExpression _ (Plus (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x + valueOf y)
getExpression _ (Minus (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x - valueOf y)
getExpression _ (Times (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x * valueOf y)
getExpression _ (Div (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x `quot` valueOf y)
getExpression _ (Mod (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x `rem` valueOf y)
getExpression _ (Plus (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x + valueOf y)
getExpression _ (Minus (NumValue x) (NumValue y)) =
  generateConstant C (valueOf x - valueOf y)
getExpression st (Plus x y) =
  getValue st C x ++ getValue st D y ++ [SWAP C, ADD D]
getExpression st (Minus x y) =
  getValue st C x ++ getValue st D y ++ [SWAP C, SUB D]
getExpression _ _ = undefined
