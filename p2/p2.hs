{-# LANGUAGE GADTs, FlexibleContexts #-}
import Debug.Trace
-- AST Definition
data KUTypeLang where
    TNum :: KUTypeLang
    TBool :: KUTypeLang 
    deriving (Show,Eq)

data KULang where
    Num :: Int -> KULang 
    Boolean :: Bool -> KULang
    Plus :: KULang -> KULang -> KULang 
    Minus :: KULang -> KULang -> KULang  
    Mult :: KULang -> KULang -> KULang  
    Div :: KULang -> KULang -> KULang   
    Exp :: KULang -> KULang -> KULang 
    And :: KULang -> KULang -> KULang   
    Or :: KULang -> KULang -> KULang  
    Leq :: KULang -> KULang -> KULang  
    IsZero :: KULang -> KULang  
    If :: KULang -> KULang -> KULang -> KULang  
    Between :: KULang -> KULang -> KULang -> KULang  
    deriving (Show,Eq)

-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: Type Inference

-- Exercise 1
evalMonad :: KULang -> Maybe KULang
evalMonad (Num x) = if x < 0 then Nothing else return (Num x) 
evalMonad (Boolean b) = Just (Boolean b)
evalMonad (Plus l r) = do {(Num l') <- evalMonad l;
                            (Num r') <-evalMonad r;
                            return (Num(l' + r'));}
evalMonad (Minus l r) = do {(Num l') <- evalMonad l;
                            (Num r') <- evalMonad r;
                            if (l'-r')<0 then Nothing else return (Num (l'-r'))}
evalMonad (Div n d) = do {(Num n') <- evalMonad n;
                            (Num d') <- evalMonad d;
                            if d' == 0 then Nothing else return (Num (n' `div` d'))}
evalMonad (Mult l r) = do {(Num l') <-evalMonad l;
                            (Num r') <-evalMonad r;
                            return (Num (l' * r'))}
evalMonad (Exp x n) = do {(Num x') <- evalMonad x;
                            (Num n') <- evalMonad n;
                            return (Num (x' ^ n'))}
evalMonad (And l r) = do {(Boolean l')<- evalMonad l;
                          (Boolean r')<- evalMonad r;
                          return (Boolean (l' && r'))}
evalMonad (Or l r) = do {(Boolean l')<- evalMonad l;
                         (Boolean r')<- evalMonad r;
                         return (Boolean (l' || r'))}
evalMonad (Leq l r) = do {(Num l')<- evalMonad l;
                          (Num r')<- evalMonad r;
                          return(Boolean(l'<=r'))}
evalMonad (IsZero x) = do {(Num x')<- evalMonad x;
                            return(Boolean(x' == 0))} 
evalMonad (If c t e) = do {(Boolean c')<- evalMonad c;
                            if c' then evalMonad t else evalMonad e}
evalMonad (Between s m e) = do {(Num s')<-evalMonad s;
                                (Num m')<-evalMonad m;
                                (Num e')<-evalMonad e;
                                return(Boolean(s'<m'&&m'<e'))}
evalMonad _ = Nothing

-- Exercise 2
typeofMonad :: KULang -> Maybe KUTypeLang
typeofMonad (Num n) = if n>=0 then return TNum else Nothing
typeofMonad (Boolean b) = return TBool
typeofMonad (Plus l r) = do {TNum <- typeofMonad l;
                             TNum <- typeofMonad r;
                             return TNum}
                             
typeofMonad (Minus l r) = do {TNum <- typeofMonad l;
                              TNum <- typeofMonad r;
                              return TNum}

typeofMonad (Mult l r) = do {TNum <- typeofMonad l;
                             TNum <- typeofMonad r;
                             return TNum}

typeofMonad (Div n d) = do {TNum <- typeofMonad n;
                            TNum <- typeofMonad d;
                            return TNum}

typeofMonad (Exp x n) = do {TNum <- typeofMonad x;
                            TNum <- typeofMonad n;
                            return TNum}

typeofMonad (And l r) = do {TBool<-typeofMonad l;
                            TBool<-typeofMonad r;
                            return TBool}

typeofMonad (Or l r) = do {TBool<-typeofMonad l;
                            TBool<-typeofMonad r;
                            return TBool}

typeofMonad (Leq l r) = do {TNum<-typeofMonad l;
                            TNum<-typeofMonad r;
                            return TBool}

typeofMonad (IsZero x) = do{TNum<-typeofMonad x;
                            return TBool}

typeofMonad (If c t e) = do{TBool<-typeofMonad c;
                            t'<- typeofMonad t;
                            e'<- typeofMonad e;
                            if t' == e' then return t' else Nothing}

typeofMonad (Between s m e) = do{TNum<- typeofMonad s;
                                 TNum<- typeofMonad m;
                                 TNum<- typeofMonad e;
                                 return TBool}

typeofMonad _ = Nothing

-- Exercise 3
interpTypeEval :: KULang -> Maybe KULang
interpTypeEval x = do{x'<- typeofMonad x;
                      evalMonad x}
interpTypeEval _ = Nothing

-- Part 2: Optimizer

-- Exercise 1
optimize :: KULang -> KULang
optimize (Num n) = Num n
optimize (Boolean b) = Boolean b
optimize (Plus l (Num 0)) = (optimize l)
optimize (Plus (Num 0) r) = optimize r
optimize (If (Boolean True) t e) = optimize t
optimize (If (Boolean False) t e) = optimize e

-- Exercise 2
interpOptEval :: KULang -> Maybe KULang
interpOptEval x = let optimizedX = optimize x in evalMonad optimizedX 
interpOptEval _ = Nothing

-- test functions

when :: Show a => Bool -> a -> IO ()
when True x = print x
when False _ = return ()

main :: IO ()
main = do
       -- Basic tests
    let interval1 = (interpTypeEval (Num 0) == Just (Num 0)) 
    let interval2 = (interpTypeEval (Num 10) == Just (Num 10))
    let interval3 = (interpTypeEval (Boolean True) == Just (Boolean True)) 
    let interval4 = (interpTypeEval (Boolean False) == Just (Boolean False))  
    let interval5 = (interpTypeEval (Plus (Num 1) (Num 2)) == Just (Num 3))
    let interval6 = (interpTypeEval (Minus (Num 5) (Num 2)) == Just (Num 3))
    let interval6 = (interpTypeEval (Minus (Num 5) (Num 7)) == Nothing)
    let interval7 = (interpTypeEval (Mult (Num 2) (Num 3)) == Just (Num 6))
    let interval8 = (interpTypeEval (Div (Num 10) (Num 2)) == Just (Num 5))
    let interval9 = (interpTypeEval (Exp (Num 2) (Num 3)) == Just (Num 8)) 
    let interval10 = (interpTypeEval (And (Boolean True) (Boolean False)) == Just (Boolean False)) 
    let interval11 = (interpTypeEval (Or (Boolean True) (Boolean False)) == Just (Boolean True)) 
    let interval12 = (interpTypeEval (Leq (Num 5) (Num 10)) == Just (Boolean True)) 
    let interval13 = (interpTypeEval (IsZero (Num 0)) == Just (Boolean True)) 
    let interval14 = (interpTypeEval (If (Boolean True) (Num 1) (Num 2)) == Just (Num 1)) 
    let interval15 = (interpTypeEval (Between (Num 5) (Num 10) (Num 15)) == Just (Boolean True)) 


    -- Tests with wrong types
    let interval16 = (interpTypeEval (Plus (Num 1) (Boolean True)) == Nothing) 
    let interval17 = (interpTypeEval (Minus (Boolean False) (Num 2)) == Nothing) 
    let interval18 = (interpTypeEval (Mult (Num 2) (Boolean True)) == Nothing) 
    let interval19 = (interpTypeEval (Div (Num 10) (Boolean False)) == Nothing)
    let interval20 = (interpTypeEval (Exp (Boolean True) (Num 3)) == Nothing) 
    let interval21 = (interpTypeEval (And (Boolean True) (Num 2)) == Nothing) 
    let interval22 = (interpTypeEval (Or (Num 1) (Boolean False)) == Nothing) 
    let interval23 = (interpTypeEval (Leq (Boolean True) (Num 10)) == Nothing) 
    let interval24 = (interpTypeEval (IsZero (Boolean False)) == Nothing) 
    let interval25 = (interpTypeEval (If (Num 1) (Boolean True) (Boolean False)) == Nothing) 
    let interval26 = (interpTypeEval (Between (Num 10) (Boolean True) (Num 15)) == Nothing) 

    -- Tests with negative numbers
    let interval27 = (typeofMonad (Num (-1)) == Nothing) 
    let interval28 = (typeofMonad (Plus (Num 1) (Num (-2))) == Nothing) 

--  addition with zero on the left
    let interval29 = interpOptEval (Plus (Num 0) (Num 123)) == Just (Num 123)

--  addition with zero on the right
    let interval30 = interpOptEval (Plus (Num 42) (Num 0)) == Just ( Num 42 )
-- if optimizations
    let interval31 = interpOptEval ((If (Boolean True) (Num 5) (Num 0))) == Just ( Num 5 )
    let interval32 = interpOptEval ((If (Boolean False) (Num 5) (Num 0))) == Just ( Num 0 )

    print interval1
    print interval2
    print interval3
    print interval4
    print interval5
    print interval6
    print interval7
    print interval8
    print interval9
    print interval10
    print interval11
    print interval12
    print interval13
    print interval14
    print interval15
    print interval16
    print interval17
    print interval18
    print interval19
    print interval20
    print interval21
    print interval22
    print interval23
    print interval24
    print interval25
    print interval26
    print interval27
    print interval28
    print interval29
    print interval30
    print interval31
    print interval32







