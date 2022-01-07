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
getExp (Plus (NumValue x) (NumValue y)) = 