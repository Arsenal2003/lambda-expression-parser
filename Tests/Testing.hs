module Tests.Testing where

import Control.Exception
import System.IO.Unsafe
import Data.List (sort)

--- type declaration ---

data Test a = Test {
    state :: Bool,
    info :: IO a
}

instance Monad Test where
    return x = Test True (return x)
    p >>= f = Test (state p && state q) (info p >>= (\_ -> info q))
        where q = f undefined

instance Applicative Test where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return $ f x

instance Functor Test where
    fmap f px = do
        x <- px
        return $ f x

--- type declaration over ---

test :: String -> Bool -> Test ()
test msg True = Test True $ putStrLn $ "\ESC[32m" ++ "[PASSED]" ++ "\ESC[0m " ++ msg
test msg False = Test False $ putStrLn $ "\ESC[91m" ++ "[FAILED]" ++ "\ESC[0m " ++ msg

test_eq :: (Show a, Eq a) => String -> a -> a -> Test ()
test_eq msg a b | a == b = test msg True
                | a /= b = Test False $ do
                      info $ test msg False
                      putStrLn $ "expected: " ++ (show b)
                      putStrLn $ "found: " ++ (show a)

test_eqs :: (Show a, Ord a) => String -> [a] -> [a] -> Test ()
test_eqs msg a b = test_eq msg (sort a) (sort b)

test_suite :: String -> Test () -> Integer -> IO ()
test_suite name tests points = catch (f tests) handle 
  where 
    f (Test True test_io) = do
        putStrLn $ "\ESC[32m" ++ "[PASSED SUITE]" ++ "\ESC[0m " ++ name 
        test_io
        putStrLn $ "+" ++ (show points) ++ " points"
    f (Test False test_io) = do
        putStrLn $ "\ESC[91m" ++ "[FAILED SUITE]" ++ "\ESC[0m " ++ name
        test_io
    handle (ErrorCall e) = putStrLn $ "\ESC[91m" ++ "Error" ++ " \ESC[0mat " ++ name ++ ": " ++ e
