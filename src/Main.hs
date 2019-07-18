module Main where
import System.Environment (getArgs)
import Parser

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run []    = putStrLn "usage: dragula <file>"
run (x:_) = pl0Parser x
