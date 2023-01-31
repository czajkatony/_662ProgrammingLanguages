{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language 
import Text.ParserCombinators.Parsec.Expr 
import Text.ParserCombinators.Parsec.Token 

-- Abstract Syntax Definition
data KULang where
 Num :: Int -> KULang
 Plus :: KULang -> KULang -> KULang
 Minus :: KULang -> KULang -> KULang
 Mult :: KULang -> KULang -> KULang
 Div :: KULang -> KULang -> KULang
 Exp :: KULang -> KULang -> KULang
 deriving (Show,Eq)

-- Exercise 1
evalErr :: KULang -> Int
-- Error if number is negative
evalErr (Num x) = if x<0 then error "negative num not allowed" else x
evalErr (Plus l r) = (evalErr l) + (evalErr l)
-- Error if two positive numbers and r is > l
evalErr (Minus l r) = let x = (evalErr l) - (evalErr r) in if x<0 then error "Subtraction err" else x
evalErr (Mult l r) = (evalErr l) * (evalErr r)
-- Error if d == 0
evalErr (Div n d) = let x = (evalErr d) in if x == 0 then error "Divide by zero error" else (evalErr n) `div` (evalErr d)
evalErr (Exp x n) = (evalErr x) ^ (evalErr n)
evalErr _ = 0
-------------------------------------------------------------------------------

--Exercise 2
evalMaybe :: KULang -> Maybe Int 
evalMaybe (Num x) = if x<0 then Nothing else Just x
evalMaybe (Plus l r) = case (evalMaybe l) of
                            Just x -> case (evalMaybe r) of
                                Just y -> Just (x + y)
                                Nothing -> Nothing
                            Nothing -> Nothing
evalMaybe (Minus l r) = case (evalMaybe l) of
                            Just x -> case (evalMaybe r) of
                                Just y -> if (x-y)<0 then Nothing else Just (x-y)
                                Nothing -> Nothing
                            Nothing -> Nothing
evalMaybe (Mult l r) = case (evalMaybe l) of
                            Just x -> case (evalMaybe r) of
                                Just y -> Just (x * y)
                                Nothing -> Nothing
                            Nothing -> Nothing
evalMaybe (Div n d) = case (evalMaybe n) of
                        Just x-> case (evalMaybe d) of
                            Just y-> if y == 0 then Nothing else Just (x `div` y)
                            Nothing -> Nothing
                        Nothing -> Nothing
evalMaybe (Exp x n) = case (evalMaybe x) of
                            Just a -> case (evalMaybe n) of
                                Just b -> Just (a ^ b)
                                Nothing -> Nothing
                            Nothing -> Nothing
evalMaybe _ = Nothing

-- Exercise 3
evalMonad :: KULang -> Maybe Int 
evalMonad (Num x) = if x < 0 then Nothing else return x
evalMonad (Plus l r) = do {l' <- evalMonad l;
                            r' <-evalMonad r;
                            return (l' + r');}
evalMonad (Minus l r) = do {l' <- evalMonad l;
                            r' <- evalMonad r;
                            if r'>l' then Nothing else return (l'-r')}
evalMonad (Div n d) = do {n' <- evalMonad n;
                            d' <- evalMonad d;
                            if d' == 0 then Nothing else return (n' `div` d')}
evalMonad (Mult l r) = do {l' <-evalMonad l;
                            r' <-evalMonad r;
                            return (l' * r')}
evalMonad (Exp x n) = do {x' <- evalMonad x;
                            n' <- evalMonad n;
                            return (x' ^ n')}
evalMonad _ = Nothing

-- Exercise 4
interpret :: String -> Maybe Int 
interpret x = evalMonad (parseKULang x) 
interpret _ = Nothing

-- KULang Parser

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedOpNames = [ "+","-","*","^","/"]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser KULang
expr = buildExpressionParser operators term

operators = [
                [inFix "*" Mult AssocLeft, 
                inFix "/" Div AssocLeft , 
                inFix "+" Plus AssocLeft , 
                inFix "-" Minus AssocLeft, 
                inFix "^" Exp AssocLeft]
            ]
  
numExpr :: Parser KULang 
numExpr = do i <- integer lexer
             return (Num (fromInteger i))
                     

term = parens lexer expr
       <|> numExpr

-- Parser invocation
-- Call parseKULang to parse a string into the KULang data structure.

parseKULang = parseString expr

-- Main function and testing
-- testCases = ["1+2","2/1","4*4","10-5","2^2"]
-- map evalErr testCases
-- map evalMaybe testCases
-- map evalMonad testCases

--TO DO: Create fully robust test cases, make them work for all.
main :: IO()
main = do
    let testCases =  ["1+2","2/1","4*4","10-5","2^2", "5/0", "9-10","2^0"]
    let results = map interpret testCases
    print results







