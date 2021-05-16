module Lib
    ( someFunc
    ) where

data TokenType = TokLeftPar | TokRightPar | TokConstant | TokVar | TokOp deriving(Show, Eq)

type Token = (TokenType, String)

--1
stringToToken :: String -> Token
stringToToken (x:xs)
 | singleStringToToken(first )

  
singleStringToToken :: String -> Token
singleStringToToken x
  | isDigit x = (TokConstant, x)
  | x == "(" = (TokLeftPar, "(")
  | x == ")" = (TokRightPar, ")")
  | isOperator x = (TokOp, x)
  | otherwise = (TokVar, [x])



isOperator :: String -> Bool
isOperator x = x == "+" || x == "-" || x == "*" || x == "/"

isDigit :: String -> Bool
isDigit x = x == "1" || x == "2" || x == "3" || x == "4" || x == "5" || x == "6" || x == "7" || x == "9" || x == "0"


someFunc :: IO ()
someFunc = putStrLn "someFunc"

