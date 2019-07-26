module IRGen (generateIR,
              sym,
              env,
              decs,
              const_env,
              procedures,
              generateDecs,
              removeNoops) where

import Control.Monad.State
import qualified Data.Map.Lazy as Map
import Data.Maybe

import qualified AST
import qualified IR

initialState = GenState {
    sym = 0,
    env = Map.empty,
    decs = Map.empty,
    const_env = Map.empty,
    procedures = []
}

data GenState = GenState {
    sym        :: Integer,
    env        :: Map.Map String String,
    decs       :: Map.Map String Integer,
    const_env  :: Map.Map Integer String,
    procedures :: [IR.Line]
}
    deriving (Show)

generateIR :: AST.PL0 -> ([IR.Line], GenState)
generateIR p = runState (genPL0 p) initialState

genPL0 :: AST.PL0 -> State GenState [IR.Line]
genPL0 (AST.Program p) = do
    p' <- genPL0 p
    return $ p' ++ [IR.Line Nothing IR.HALT]

genPL0 (AST.Block c v p s) = do
    genConstsPL0 c
    genVarsPL0 v
    procs <- mapM genPL0 p
    stmts <- genPL0 s
    state <- get
    put state { procedures = ((concat procs) ++) . procedures $ state }
    return $ stmts

genPL0 (AST.Procedure s body) = do
    state <- get
    let label = IR.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    state <- get
    put state { env = Map.insert s label . env $ state }
    body' <- genPL0 body
    return $ [IR.Line (Just label) IR.STARTFUNC] ++
             body' ++
             [IR.Line Nothing IR.RET]

genPL0 (AST.Begin stmts) = do
    stmts' <- mapM genPL0 stmts
    return $ concat stmts'

genPL0 (AST.If cond stmt) = do
    state <- get
    let label = IR.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    cond' <- genExpr cond
    stmt' <- genPL0 stmt
    return (cond' ++
            [IR.Line Nothing (IR.JMZ label)] ++
            stmt' ++
            [IR.Line (Just label) IR.NOOP])

genPL0 (AST.While cond stmt) = do
    state <- get
    let forwardLabel = IR.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    state <- get
    let backwardLabel = IR.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    cond' <- genExpr cond
    stmt' <- genPL0 stmt
    return ([IR.Line (Just backwardLabel) IR.NOOP] ++
            cond' ++
            [IR.Line Nothing (IR.JMZ forwardLabel)] ++
            stmt' ++
            [IR.Line Nothing (IR.JMP backwardLabel)] ++
            [IR.Line (Just forwardLabel) IR.NOOP])

genPL0 (AST.WriteLn e) = do
    e' <- genExpr e
    return (e' ++ [IR.Line Nothing IR.WRITE])

genPL0 (AST.Call s) = do
    state <- get
    let (Just label) = Map.lookup s (env state)
    return [IR.Line Nothing (IR.CALL label)]

genPL0 (AST.Assign s e) = do
    e' <- genExpr e
    state <- get
    let (Just label) = Map.lookup s (env state)
    return $ e' ++ [IR.Line Nothing (IR.STORE label)]

genVarsPL0 :: Maybe AST.PL0 -> State GenState ()
genVarsPL0 Nothing = return ()
genVarsPL0 (Just (AST.VarDec decs)) = mapM_ genVarPL0 decs

genVarPL0 :: AST.PL0 -> State GenState ()
genVarPL0 (AST.Var v) = do
    state <- get
    let label = IR.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    state <- get
    put state { env = Map.insert v label . env $ state }
    state <- get
    put state { decs = Map.insert label 0 . decs $ state }

genConstsPL0 :: Maybe AST.PL0 -> State GenState ()
genConstsPL0 Nothing = return ()
genConstsPL0 (Just (AST.ConstDec decs)) = mapM_ genConstPL0 decs

genConstPL0 :: AST.PL0 -> State GenState ()
genConstPL0 (AST.Const s i) = do
    state <- get
    let label = IR.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    state <- get
    put state { env = Map.insert s label . env $ state }
    state <- get
    put state { decs = Map.insert label i . decs $ state }

genExpr :: AST.Expr -> State GenState [IR.Line]
genExpr (AST.Mult l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IR.Line Nothing IR.MUL]

genExpr (AST.Div l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IR.Line Nothing IR.DIV]

genExpr (AST.Add l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IR.Line Nothing IR.ADD]

genExpr (AST.LT l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IR.Line Nothing IR.LT]

genExpr (AST.LTE l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IR.Line Nothing IR.LTE]

genExpr (AST.GT l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IR.Line Nothing IR.GT]

genExpr (AST.GTE l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IR.Line Nothing IR.GTE]

genExpr (AST.NE l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IR.Line Nothing IR.NEQ]

genExpr (AST.Eq l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IR.Line Nothing IR.EQ]

genExpr (AST.Ident s) = do
    state <- get
    let (Just label) = Map.lookup s (env state)
    return [IR.Line Nothing (IR.LOAD label)]

genExpr (AST.IntConst i) = do
    state <- get
    let cenv = const_env state
    let const = Map.lookup i cenv
    case const of
        (Just label) -> return [IR.Line Nothing (IR.LOAD label)]
        Nothing      -> do let label = IR.mkLabel (sym state)
                           put state { sym = succ . sym $ state,
                                       const_env = Map.insert i label (const_env state),
                                       decs = Map.insert label i (decs state) }
                           return [IR.Line Nothing (IR.LOAD label)]

removeNoops :: [IR.Line] -> [IR.Line]
removeNoops []     = []
removeNoops (x:xs) =
    if null xs
        then x:removeNoops xs
        else let (IR.Line label cmd) = head xs
             in case x of
                    (IR.Line l IR.NOOP) -> (IR.Line l cmd):(removeNoops (tail xs))
                    _                   -> x:(removeNoops xs)

generateDecs :: Map.Map String Integer -> [IR.Line]
generateDecs m =
    let list = Map.toList m
    in map (\(s, v) -> IR.Line (Just s) (IR.DEC v)) list
