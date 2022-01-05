module MyFuns.Numbers (
    generateIncCode,
    valueOf

) where
import MyFuns.SimpleLanguage
import Gramma.Abs 


remNumPart :: String -> String
remNumPart s =  [x | x <- s, not (x `elem` "Number \"\'")]

textOfNumber :: Number -> String
textOfNumber n =  ( remNumPart $ show n )

valueOf :: Number -> Integer
valueOf s =   read (textOfNumber s)::Integer

generateIncCode :: Integer -> [OpCode] -> [OpCode]
generateIncCode x  y 
    | x > 0  = [INC A] ++ ( (generateIncCode (x-1)) y )
    | otherwise = y
