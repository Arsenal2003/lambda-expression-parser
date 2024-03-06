module Tests.ParserTest (parser_test) where

import Expr
import Parser
import Tests.Testing
import Tests.Examples

parse_expr_test :: Test ()
parse_expr_test = do
    test_eq "parse variable" (parse_expr "x") vx
    test_eq "parse function" (parse_expr "\\x.y") (f "x" vy)
    test_eq "parse complex function" (parse_expr "\\x.(x y)") (f "x" $ a vx vy)
    test_eq "parse simple application" (parse_expr "x y") (a vx vy)
    test_eq "parse multiple applications" (parse_expr "x y z t") (a (a (a vx vy) vz) vt)
    test_eq "parse complex application" (parse_expr "x (y z)") (a vx (a vy vz))
    test_eq "parse M-combinator" (parse_expr "\\x.(x x)") m
    test_eq "parse I-combinator" (parse_expr "\\x.x") i
    test_eq "parse K-combinator" (parse_expr "\\x.\\y.x") k
    test_eq "parse KI-combinator" (parse_expr "\\x.\\y.y") ki
    test_eq "parse C-combinator" (parse_expr "\\f.\\x.\\y.(f y x)") c
    test_eq "parse B-combinator" (parse_expr "\\f.\\g.\\x.(f (g x))") b
    test_eq "parse B1-combinator" (parse_expr "\\f.\\g.\\x.\\y.(f (g x y))") b1
    test_eq "parse Y-combinator" (parse_expr "\\f.((\\x.(f (x x))) (\\x.(f (x x))))") y

parser_test :: IO ()
parser_test = do
    putStrLn $ "2. Parsing"
    test_suite "2.1. parse expressions" parse_expr_test 50
