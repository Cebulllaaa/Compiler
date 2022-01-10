module MyFuns.Numbers (
  generateIncCode,
  valueOf,
  getExpression,
  getValue,
  getIdAddr
) where
import MyFuns.SimpleLanguage
import Gramma.Abs
import Data.Text as T
import Data.List (genericReplicate)

valueOf :: Number -> Integer
valueOf (Number txt) = read $ unpack txt

generateIncCode :: Integer -> [OpCode] -> Reg -> [OpCode]
generateIncCode x  y r
  | x > 0  = [INC r] ++ ( (generateIncCode (x-1)) y r)
  | otherwise = y

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

getValue :: Reg -> Value -> [OpCode]
getValue reg (NumValue x) = getNumber reg x
getValue reg (IdValue x) = getIdValue reg x

getIdValue :: Reg -> Identifier -> [OpCode]
getIdValue reg = undefined

getIdAddr :: Reg -> Identifier -> [OpCode]
getIdAddr reg = undefined

getExpression :: Expression -> [OpCode]
getExpression (ValueExpr x) = getValue A x
getExpression (Plus (NumValue x) (NumValue y)) = [RESET D] ++
  (generateIncCode ((valueOf x)+ (valueOf y)-1)[INC D]  D)
getExpression (Minus (NumValue x) (NumValue y)) = [RESET D] ++
  (generateIncCode ((valueOf x) - (valueOf y) -1)  [INC D] D)
getExpression (Times (NumValue x) (NumValue y)) = [RESET D] ++
  (generateIncCode ((valueOf x) * (valueOf y)-1 )  [INC D] D)
getExpression (Div (NumValue x) (NumValue y)) = [RESET D] ++
  (generateIncCode ((valueOf x) `div` (valueOf y) -1)  [INC D] D)
getExpression (Mod (NumValue x) (NumValue y)) = [RESET D] ++
  (generateIncCode ((valueOf x) `mod`  (valueOf y)-1)  [INC D] D)
