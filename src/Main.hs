module Main where
import System.Environment (getArgs)
import Text.Printf
import Parser
import IRGenPDP
-- import CodeGen
-- import qualified VM

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run []    = putStrLn "usage: dragula <file>"
run (x:_) = do
    result <- pl0Parser x
    case result of
        Left err -> print err
        Right pl0 -> do let (ir, state) = generateIR pl0
                        let procs = procedures state
                        let ir' = removeNoops ir
                        let procs' = removeNoops procs
                        let decs' = generateDecs . decs $ state
                        let code = ir' ++ decs' ++ procs'
                        -- let code' = generateCode code
                        -- print pl0
                        -- print ir
                        mapM_ (print) code
                        -- state <- VM.load code'
                        -- VM.run state
