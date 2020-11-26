{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE BlockArguments  #-}


module Expression where

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

-- Our Lisp expression
data Expr = LInt Integer |
        LSymbol String |
        LFn LFunction FunctionSignature |
        LSpecial LFunction FunctionSignature |
        LList [Expr]

-- The function type
type FunctionSignature = [String]
type LFunction = LResult

-- Context in which expressions will be evaluated
type SymbolTable = Map.Map String Expr
data Context = Ctx SymbolTable (Maybe Context)

-- Helper context functions
updateSymbol s eval_e = modify (\(Ctx sym_table parentCtx)->(Ctx (Map.insert s eval_e sym_table)) parentCtx)

updateSymbolInParent s eval_e = modify (\(Ctx sym_table parent_ctx)->(Ctx sym_table (updatedCtx parent_ctx)))
    where updatedCtx (Just (Ctx sym_table ctx)) = (Just (Ctx (Map.insert s eval_e sym_table) ctx))

pushContext ctx = Ctx Map.empty (Just ctx)

popContext ctx@(Ctx _ Nothing) = ctx

popContext (Ctx _ (Just parentCtx)) = parentCtx

-- A state monad that holds a context and an evaluation result
type LError =  ExceptT String IO
type LResult = StateT Context LError Expr

-- Printing the expression
instance Show Expr where
    show (LInt x) = show x
    show (LSymbol x) = x
    show (LFn _ _) = "<function>"
    show (LSpecial _ _) = "<special-form>"
    show (LList x) = "(" ++ unwords (map show x) ++ ")"

-- Parsing the expression
parseInteger = do   
                    sign <- option "" (string "-")
                    number <- many1 digit
                    return $ LInt (read (sign++number))

parseSymbol = do 
                 f <- firstAllowed
                 r <- many (firstAllowed <|> digit)
                 return $ LSymbol (f:r)
    where firstAllowed = oneOf "+-*/" <|> letter

parseExprAux = (try parseInteger) <|> (try parseSymbol) <|> (try parseList)

parseList = do 
              char '(' ; skipMany space
              x <- parseExprAux `sepEndBy` (many1 space)
              char ')'
              return $ LList x

parseExpr = do 
              skipMany space
              x <- parseExprAux
              skipMany space ; eof
              return x

parse :: String -> LResult
parse source = case (Text.ParserCombinators.Parsec.parse parseExpr "" source) of
                 Right x -> return x
                 Left e -> throwError $ show e
