module Tests.MacroTest (macro_test) where

import Expr
import Parser
import Lambda
import Tests.Testing
import Tests.Examples

test_ctx :: [(String, Expr)]
test_ctx = [("m", m), ("and", land), ("true", ltrue), ("false", lfalse), ("mm", (macro "m"))]

eval_macro_test :: Test ()
eval_macro_test = do
    test_eq "eval no macro" (evalMacros test_ctx f0) f0 
    test_eq "eval macro" (evalMacros test_ctx (macro "m")) m
    test_eq "eval macro application" (evalMacros test_ctx (a (macro "m") (macro "m"))) (a m m)
    test_eq "eval macro function" (evalMacros test_ctx (f "x" (macro "m"))) (f "x" m)
    test_eq "eval complex macro"
            (evalMacros test_ctx (a (a (macro "and") (macro "true")) (macro "false")))
            (a (a land ltrue) lfalse)
    test_eq "eval macro in macro" (evalMacros test_ctx (macro "mm")) m

parse_macro_test :: Test ()
parse_macro_test = do
    test_eq "parse macro" (parse_expr "$x") (macro "x")
    test_eq "parse macro naming" (parse_expr "$haskell") (macro "haskell")
    test_eq "parse application macros" (parse_expr "$x $y") (a (macro "x") (macro "y"))
    test_eq "parse function macro" (parse_expr "\\x.$body") (f "x" (macro "body"))
    test_eq "parse 'complex' expression" 
            (parse_expr "\\x.\\y.$body $param") 
            (a (f "x" $ f "y" (macro "body")) (macro "param"))
    test_eq "parse macro expression"
            (parse_expr "$and $true $false")
            (a (a (macro "and") (macro "true")) (macro "false"))

macro_test :: IO ()
macro_test = do
    putStrLn $ "3. Macros"
    test_suite "3.1. evaluate macros" eval_macro_test 10
    test_suite "3.2. parse macros" parse_macro_test 5
