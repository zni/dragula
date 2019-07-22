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
        Right pl0 -> do let (ir, state) = generateCode pl0
                        mapM_ print ir
                        print state
