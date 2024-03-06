module Tests.CodeTest (code_test) where

import Expr
import Parser
import Lambda
import Tests.Testing
import Tests.Examples

eval_code_test :: Test ()
eval_code_test = do
    test_eq "eval AND TRUE FALSE" (evalCode reduceN p0) a0 
    test_eq "eval multiple expressions" (evalCode reduceN p1) a1
    test_eq "eval override macro" (evalCode reduceN p2) a2 
    test_eq "eval numeric expressions" (evalCode reduceN p3) a3

parse_code_test :: Test ()
parse_code_test = do
    test_eq "parse evaluate expression" (parse_code "\\x.x \\x.x") (Evaluate (a i i))
    test_eq "parse assign expression" (parse_code "i = \\x.x") (Assign "i" i)
    test_eq "parse whitespaces" (parse_code "m  =   \\x.(x x)") (Assign "m" m) 
    test_eq "parse no whitespaces" (parse_code "m=\\x.(x x)") (Assign "m" m)
    test_eq "parse KI-combinator" (parse_code "ki = \\x.\\y.y") (Assign "ki" ki)
    test_eq "parse C-combinator" (parse_code "c = \\f.\\x.\\y.(f y x)") (Assign "c" c) 
    test_eq "parse B1-combinator" (parse_code "b = \\f.\\g.\\x.\\y.(f (g x y))") (Assign "b" b1)

code_test :: IO ()
code_test = do
    putStrLn $ "3. Code"
    test_suite "3.1. evaluate code" eval_code_test 10
    test_suite "3.2. parse code" parse_code_test 5
