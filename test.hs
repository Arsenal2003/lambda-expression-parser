import Tests.LambdaTest
import Tests.ParserTest
import Tests.MacroTest
import Tests.CodeTest

import System.Environment

main :: IO ()
main = do
    putStrLn "Running tests..."

    args <- getArgs
    case args of
        ["lambda"] -> lambda_test
        ["parser"] -> parser_test
        ["macro"] -> macro_test
        ["code"] -> code_test
        [] -> do
            lambda_test
            parser_test
            macro_test
            code_test

        _ -> putStrLn "Usage: available options: [lambda|parser|macro|code]"

    putStrLn "Done."
