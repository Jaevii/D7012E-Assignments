module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

-- Computes a and b, only returns the result of b
(-#) :: Parser a -> Parser b -> Parser b
m -# n = (#) m n >-> snd

-- Computes a and b, only returns the result of a
(#-) :: Parser a -> Parser b -> Parser a
m #- n = (#) m n >-> fst

-- iterates the isSpace function on each char in the string
spaces :: Parser String
spaces = iter $ char ? isSpace -- same as iter (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces

-- checks if a given char is an alphabetic letter (A-z) or not
letter :: Parser Char
letter = char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

-- returns a given number of chars from a string
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons

-- returns true if some string matches a given string, fails quietly
accept :: String -> Parser String
accept w = token (chars (length w)) ? (==w)

-- same as accept but also returns error if the strings do not match
require :: String -> Parser String
require w = accept w ! err ("expecting: "++w)

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

