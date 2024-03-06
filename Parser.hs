module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    return = undefined
    (>>=) = undefined

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

--- type declaration over ---

-- TODO 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr = undefined

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code = undefined
