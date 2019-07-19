module Main where
import System.Environment (getArgs)
import Parser
import CodeGen

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run []    = putStrLn "usage: dragula <file>"
run (x:_) = do
    result <- pl0Parser x
    case result of
        Left err -> print err
        Right pl0 -> genPL0 pl0
