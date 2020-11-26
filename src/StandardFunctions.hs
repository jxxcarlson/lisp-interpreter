
module StandardFunctions where

import Expression
import Eval
import qualified Data.Map as Map
import Control.Monad.State

-- Standard functions we expose
lispArithmetic f  = do 
                      (LList args) <- getSymbol "..."
                      lispBinary f args

lispBinary :: (Integer->Integer->Integer) -> [Expr] -> LResult
lispBinary op args = do return $ foldl1 (lispBinaryAux op) args
     where lispBinaryAux op (LInt i) (LInt j) = LInt (i `op` j)

-- Equality
lispEq = do 
           (LList args) <- getSymbol "..."
           return $ foldl1 (\(LInt a) (LInt b) -> LInt(if a == b then 1 else 0)) args

-- A function to modify the context
lispSetArgs = ["symbol", "value"]
lispSet = do 
            [(LSymbol s), e] <- getSymbols lispSetArgs
            eval_e <- eval e
            updateSymbolInParent s eval_e
            return eval_e

-- Conditionals
lispIfArgs = ["condition", "expr1", "expr2"]
lispIf = do 
           [condExpr, expr1, expr2] <- getSymbols lispIfArgs
           eval_cond <- eval condExpr
           if (0 `notEqual` eval_cond) then eval expr1 else eval expr2
    where notEqual val1 (LInt val2) = val1 /= val2

-- Functions
lispFnArgs = ["args", "..."]
lispFn = do 
            [(LList args), (LList body)] <- getSymbols lispFnArgs
            let newFn = do 
                              evalBody <- mapM eval body
                              return $ last evalBody
            return $ LFn newFn (map (\(LSymbol arg) -> arg) args)

-- Our symbol table
initialCtx = Ctx (Map.fromList [("+", LFn (lispArithmetic (+)) ["..."]),
    ("-", LFn (lispArithmetic (-)) ["..."]),
    ("*", LFn (lispArithmetic (*)) ["..."]),
    ("/", LFn (lispArithmetic div) ["..."]),
                           ("eq", LFn lispEq ["..."]),
    ("set", LSpecial lispSet lispSetArgs),
                           ("if", LSpecial lispIf lispIfArgs),
                           ("fn", LSpecial lispFn lispFnArgs )
    ]) Nothing

-- Helper
getSymbol sym = eval $ (LSymbol sym)
getSymbols syms = mapM getSymbol syms