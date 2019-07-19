module CodeGen where

import qualified AST

genPL0 :: AST.PL0 -> IO ()
genPL0 (AST.Program p) = do
    genPL0 p

genPL0 (AST.Block c v p s) = do
    genPL0 s

genPL0 (AST.Begin stmts) = do
    mapM_ genPL0 stmts

genPL0 (AST.If cond stmt) = do
    mapExpr cond
    putStrLn "jmz"
    genPL0 stmt

genPL0 (AST.While cond stmt) = do
    mapExpr cond
    putStrLn "jmz"
    genPL0 stmt

genPL0 (AST.WriteLn e) = do
    mapExpr e
    putStrLn "write"

genPL0 (AST.Call s) = do
    putStrLn "jmp"

genPL0 (AST.Assign s e) = do
    mapExpr e
    putStrLn $ "store " ++ s

mapExpr :: AST.Expr -> IO ()
mapExpr (AST.Mult l r) = do
    mapExpr l
    mapExpr r
    putStrLn "mul"

mapExpr (AST.Div l r) = do
    mapExpr l
    mapExpr r
    putStrLn "div"

mapExpr (AST.Add l r) = do
    mapExpr l
    mapExpr r
    putStrLn "add"

mapExpr (AST.LT l r) = do
    mapExpr l
    mapExpr r
    putStrLn "lt"

mapExpr (AST.LTE l r) = do
    mapExpr l
    mapExpr r
    putStrLn "lte"

mapExpr (AST.GT l r) = do
    mapExpr l
    mapExpr r
    putStrLn "gt"

mapExpr (AST.GTE l r) = do
    mapExpr l
    mapExpr r
    putStrLn "gte"

mapExpr (AST.NE l r) = do
    mapExpr l
    mapExpr r
    putStrLn "ne"

mapExpr (AST.Eq l r) = do
    mapExpr l
    mapExpr r
    putStrLn "eq"

mapExpr (AST.Ident s) = do
    putStrLn $ "load " ++ s

mapExpr (AST.IntConst i) = do
    putStrLn $ "loadconst " ++ (show i)

