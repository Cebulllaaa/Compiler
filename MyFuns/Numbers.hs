module MyFuns.Numbers (
    generateIncCode,
    valueOf,
    getExp

) where
import MyFuns.SimpleLanguage
import Gramma.Abs 
import Data.Text as T

valueOf :: Number -> Integer
valueOf (Number txt) = read $ unpack txt

generateIncCode :: Integer -> [OpCode] -> Reg -> [OpCode]
generateIncCode x  y r
    | x > 0  = [INC r] ++ ( (generateIncCode (x-1)) y r)
    | otherwise = y

getExp :: Expression -> [OpCode]
getExp (ValueExpr x) = undefined
getExp (Plus (NumValue x) (NumValue y)) = [RESET D] ++ 
    (generateIncCode ((valueOf x)+ (valueOf y)-1)[INC D]  D) 
getExp (Minus (NumValue x) (NumValue y)) = [RESET D] ++ 
    (generateIncCode ((valueOf x) - (valueOf y) -1)  [INC D] D) 
getExp (Times (NumValue x) (NumValue y)) = [RESET D] ++ 
    (generateIncCode ((valueOf x) * (valueOf y)-1 )  [INC D] D) 
getExp (Div (NumValue x) (NumValue y)) = [RESET D] ++ 
    (generateIncCode ((valueOf x) `div` (valueOf y) -1)  [INC D] D) 
getExp (Mod (NumValue x) (NumValue y)) = [RESET D] ++ 
    (generateIncCode ((valueOf x) `mod`  (valueOf y)-1)  [INC D] D) 
