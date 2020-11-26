{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Eval where

import Expression
import qualified Data.Map as Map
import Data.List
import Control.Monad.State
import Control.Monad.Except

-- Evaluating Lisp expression
eval :: Expr -> LResult
eval (LInt n) = return (LInt n)
eval (LFn f args) = return (LFn f args)
eval (LSpecial f args) = return (LSpecial f args)
eval (LSymbol s) = do 
                        context <- get
                        lookupSymbol context
    where lookupSymbol (Ctx sym_table parentCtx) =
              if s `Map.member` sym_table
              then return (sym_table Map.! s)
              else case parentCtx of
                     Nothing -> throwError ("Symbol " ++ s ++ " is unbound.")
                     (Just parent) -> lookupSymbol parent

eval (LList (x:xs)) = do 
                        fn <- eval x
                        apply fn
      where apply (LSpecial f expectedArgs) = apply' expectedArgs xs f
            apply (LFn f expectedArgs) = 
                  do
                        args <- mapM eval xs
                        apply' expectedArgs args f
            apply' expectedArgs args f = 
                  do 
                        modify pushContext
                        applyArgsToContext expectedArgs args
                        result <- f
                        modify popContext
                        return result
            applyArgsToContext ("...":_) args = do updateSymbol "..." (LList args)
            applyArgsToContext (earg:expectedArgs) (arg:args) = do updateSymbol earg arg
                                                                   applyArgsToContext expectedArgs args
            applyArgsToContext [] _ = return ()
