module Lib
    ( someFunc
    ) where
      
data TokenType = TokLeftPar | TokRightPar | TokConstant | TokVar | TokOp deriving(Show, Eq)

type Token = (TokenType, String)


isOp :: Char -> Bool
isOp c = c == '+' || c == '-' || c == '*' || c == '/'

isConst :: Char -> Bool
isConst c = c  == '0' || c  == '1' || c  == '2' || c  == '3' ||
 c  == '4' || c  == '5' || c  == '6' || c  == '7' || c  == '8' || c  == '9'

isLeftPar :: Char -> Bool
isLeftPar c = c == '('

isRightPar :: Char -> Bool
isRightPar c = c == ')'

isVar :: Char -> Bool
isVar c = not (isOp c || isConst c || isLeftPar c || isRightPar c)

subString :: String -> Int -> String
subString str endIndex = take (endIndex + 1) str

getEndIndexConst :: String -> Int -> Int
getEndIndexConst [] lastIndex = lastIndex
getEndIndexConst str lastIndex 
  | isConst (head str) = getEndIndexConst (drop 1 str) (lastIndex + 1)
  | otherwise = lastIndex

getEndIndexVar :: String -> Int -> Int
getEndIndexVar [] lastIndex = lastIndex
getEndIndexVar str lastIndex
  | isVar (head str) = getEndIndexVar (drop 1 str) (lastIndex + 1)
  | otherwise = lastIndex

--1

deleteSpace :: String -> String
deleteSpace [] = []
deleteSpace (x:xs)
  | x == ' ' = deleteSpace xs
  | otherwise = x : deleteSpace xs

lexer :: String -> [Token]
lexer str = map stringToToken (singleTokenList (deleteSpace str))

singleTokenList :: String -> [String]
singleTokenList [] = []
singleTokenList str
  | isOp (head str) || isLeftPar (head str) || 
    isRightPar (head str) = [head str] : singleTokenList (drop 1 str)
  | isConst (head str) = let endIndexConst = getEndIndexConst (drop 1 str) 0 in
    subString str endIndexConst : singleTokenList (drop (endIndexConst + 1) str)
  | isVar (head str) = let endIndexVar = getEndIndexVar (drop 1 str) 0 in 
    subString str endIndexVar : singleTokenList (drop (endIndexVar + 1) str)

stringToToken :: String -> Token
stringToToken str = (getTokenType str, str)

getTokenType :: String -> TokenType
getTokenType (x:xs)
  | isOp x = TokOp
  | isConst x = TokConstant
  | isLeftPar x = TokLeftPar
  | isRightPar x = TokRightPar
  | isVar x = TokVar

someFunc :: IO ()
someFunc = print (lexer "(yy + 3 * (x+1)) -22 ")

