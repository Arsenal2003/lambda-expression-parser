module Tests.LambdaTest (lambda_test) where

import Expr
import Lambda 
import Tests.Testing
import Tests.Examples

free_vars_test :: Test ()
free_vars_test = do
    test_eqs "free_vars variable" (free_vars va) ["a"]
    test_eqs "free_vars simple 1" (free_vars e0) ["y"]
    test_eqs "free_vars simple 2" (free_vars e1) ["x"]
    test_eqs "free_vars application" (free_vars (a e0 e1)) ["x", "y"]
    test_eqs "free_vars complex 1" (free_vars e2) ["x", "z"]
    test_eqs "free_vars complex 2" (free_vars (a e2 e2)) ["x", "z"]
    test_eqs "free_vars Y-combinator" (free_vars y) []
    test_eqs "free_vars C-combinator" (free_vars c) []
    test_eqs "free_vars B-combinator" (free_vars b) []

reduce_test :: Test ()
reduce_test = do 
    test_eq "reduce variable" (reduce vx "x" m) (m)
    test_eq "reduce non-related variable" (reduce vy "x" m) vy
    test_eq "reduce application simple" (reduce (a vx vx) "x" m) (a m m)
    test_eq "reduce function same var" (reduce (f "x" m) "x" e1) (f "x" m)
    test_eq "reduce function" (reduce (f "y" vx) "x" m) (f "y" m)
    test_eq "reduce variable capture" 
            (reduce (f "y" (a vx vy)) "x" (f "x" vy)) 
            (f "a" (a (f "x" vy) va))

normal_test :: Test ()
normal_test = do
    test_eq "stepN \\x.y (m m)" (stepN f0) vy
    test_eq "stepN expression" (stepN f1) (norm1 !! 1)
    test_eq "stepN 2 steps" (stepN $ stepN f1) (norm1 !! 2)

normal_reduce_test :: Test ()
normal_reduce_test = do
    test_eq "reduceN \\x.y (m m)" (reduceN f0) vy
    test_eq "reduceN expression" (reduceN f1) (last norm1)
    test_eq "reduceN numeric" (reduceN (a iszero zero)) ltrue
    test_eq "reduceN numeric add" (reduceN (a (a add one) two)) three 
    test_eq "reduceAllN \\x.y (m m)" (reduceAllN f0) [f0, vy]
    test_eq "reduceAllN expression" (reduceAllN f1) norm1

applicative_test :: Test ()
applicative_test = do
    test_eq "stepA \\x.y (m m)" (stepA f0) f0
    test_eq "stepA expression" (stepA f1) (app1 !! 1)
    test_eq "stepA 2 steps" (stepA $ stepA f1) (app1 !! 2)

applicative_reduce_test :: Test ()
applicative_reduce_test = do 
    test_eq "reduceA expression" (reduceA f1) (last app1)
    test_eq "reduceA numeric" (reduceA (a iszero zero)) ltrue
    test_eq "reduceA numeric add" (reduceA (a (a add one) two)) three 
    test_eq "reduceAllA expression" (reduceAllA f1) app1

lambda_test :: IO ()
lambda_test = do
    putStrLn $ "1. Evaluation"
    test_suite "1.1. free_vars" free_vars_test 10
    test_suite "1.2. reduce" reduce_test 30
    test_suite "1.3. stepN" normal_test 5
    test_suite "1.4. reduceN, reduceAllN" normal_reduce_test 10 
    test_suite "1.5. stepA" applicative_test 5
    test_suite "1.6. reduceA, reduceAllA" applicative_reduce_test 10
