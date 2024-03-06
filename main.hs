import Lambda
import Parser
import Expr
import Tests.Examples

import System.IO

default_ctx :: [(String, Expr)]
default_ctx = [ ("true", ltrue)
              , ("false", lfalse)
              , ("and", land)
              , ("or", lor)
              , ("not", lnot)
              , ("zero", zero)
              , ("succ", nsucc)
              , ("add", add)
              , ("mult", mult)
              , ("iszero", iszero)
              , ("m", m)
              , ("i", i)
              , ("k", k)
              , ("ki", ki)
              , ("c", c)
              , ("b", b)
              , ("y", y)
              ]

main :: IO ()
main = lambda default_ctx 

lambda :: [(String, Expr)] -> IO () 
lambda ctx = do
    putStr "λ> "
    hFlush stdout
    input <- getLine
    case input of
        "exit" -> return ()
        _ -> case parse_code input of
                (Evaluate e) -> do
                    putStrLn $ show $ reduceN (evalMacros ctx e)
                    lambda ctx
                (Assign s e) -> do
                    lambda ((s, evalMacros ctx e):ctx)
