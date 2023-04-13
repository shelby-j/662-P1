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
evalErr (Num x) = if x<0 then error "Negative not allowed" else x
evalErr (Plus l r) = (evalErr l) + (evalErr r)
evalErr (Minus l r) = let x =((evalErr l) - (evalErr r)) in
 if x<0 then error "Negatives are bad" else x
evalErr (Mult l r) = (evalErr l) * (evalErr r)
evalErr (Div l r) = let x =((evalErr l) `div` (evalErr r)) in
 if evalErr(l) `rem` evalErr(r) /= 0 then error "Not Divisible" else x
evalErr (Exp b e) = (evalErr b)^(evalErr e)

--Exercise 2
evalMaybe :: KULang -> Maybe Int
evalMaybe (Num x) = if x<0 then Nothing else Just x
evalMaybe (Plus l r) = case (evalMaybe l) of
				   Just x -> case (evalMaybe r) of
				              Just y -> Just (x+y) 
				   Nothing -> Nothing
evalMaybe (Minus l r) = case (evalMaybe l) of
				   Just x -> case (evalMaybe r) of
				              Just y -> if (x-y)<0 then Nothing else Just (x-y)
				              Nothing -> Nothing
				   Nothing -> Nothing
evalMaybe (Mult l r) = case (evalMaybe l) of
				   Just x -> case (evalMaybe r) of
				              Just y -> Just (x*y)
				   Nothing -> Nothing 
evalMaybe (Div l r) = case (evalMaybe l) of
				   Just x -> case (evalMaybe r) of
				              Just y -> if (x `rem` y)/=0 then Nothing else Just (x `div` y)
				              Nothing -> Nothing
				   Nothing -> Nothing
evalMaybe (Exp l r) = case (evalMaybe l) of
				   Just x -> case (evalMaybe r) of
				              Just y -> Just (x^y)
				   Nothing -> Nothing
-- Exercise 3
evalMonad :: KULang -> Maybe Int 
evalMonad (Num x) = if x<0 then Nothing else return x
evalMonad (Plus l r) = do {l' <- evalMonad l;
                           r' <- evalMonad r;
						   return (l' + r')}
evalMonad (Minus l r) = do {l' <- evalMonad l;
                           r' <- evalMonad r;
						   if (l'-r')<0 then Nothing else return (l'-r')}
evalMonad (Mult l r) = do {l' <- evalMonad l;
                           r' <- evalMonad r;
						   return (l' * r')}
evalMonad (Div l r) = do {l' <- evalMonad l;
                          r' <- evalMonad r;
						  if (l' `rem` r')/=0 then Nothing else return (l' `div` r')}						   
evalMonad (Exp l r) = do {l' <- evalMonad l;
                          r' <- evalMonad r;
						  return (l'^r')}
						   
-- Exercise 4
interpret :: String -> Maybe Int 
interpret x = evalMonad(parseKULang x)

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

main :: IO ()
main = return ()
