-- https://www.defmacro.org/ramblings/lisp-in-haskell.html

module Lisp where

data Expr = LInt Integer |
            LSymbol String |
            LFn ([Expr]->Expr) |
            LList [Expr]

instance Show Expr where
        show (LInt x) = show x
        show (LSymbol x) = x
        show (LFn x) = "<function>"
        show (LList x) = "(" ++ unwords (map show x) ++ ")"

-- TESTING


myList = LList [LInt 1, LInt 2, LInt 3]