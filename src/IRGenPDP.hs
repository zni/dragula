module IRGenPDP (generateIR,
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
import qualified IRPDP

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
    procedures :: [IRPDP.Line]
}
    deriving (Show)

generateIR :: AST.PL0 -> ([IRPDP.Line], GenState)
generateIR p = runState (genPL0 p) initialState

genPL0 :: AST.PL0 -> State GenState [IRPDP.Line]
genPL0 (AST.Program p) = do
    p' <- genPL0 p
    return $ p' ++ [IRPDP.Line Nothing IRPDP.HALT]

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
    let label = IRPDP.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    state <- get
    put state { env = Map.insert s label . env $ state }
    body' <- genPL0 body
    return $ [IRPDP.Func (Just label)] ++
             body' ++
             [IRPDP.Line Nothing (IRPDP.RTS "PC")]

genPL0 (AST.Begin stmts) = do
    stmts' <- mapM genPL0 stmts
    return $ concat stmts'

genPL0 (AST.If cond stmt) = do
    state <- get
    let label = IRPDP.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    cond' <- genExpr cond
    stmt' <- genPL0 stmt
    return (cond' ++
            [IRPDP.Line Nothing (IRPDP.BEQ label)] ++
            stmt' ++
            [IRPDP.Line (Just label) IRPDP.NOP])

genPL0 (AST.While cond stmt) = do
    state <- get
    let forwardLabel = IRPDP.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    state <- get
    let backwardLabel = IRPDP.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    cond' <- genExpr cond
    stmt' <- genPL0 stmt
    return ([IRPDP.Line (Just backwardLabel) IRPDP.NOP] ++
            cond' ++
            [IRPDP.Line Nothing (IRPDP.BNE forwardLabel)] ++
            stmt' ++
            [IRPDP.Line Nothing (IRPDP.BR backwardLabel)] ++
            [IRPDP.Line (Just forwardLabel) IRPDP.NOP])

genPL0 (AST.WriteLn e) = do
    e' <- genExpr e
    return (e' ++ [IRPDP.Line Nothing IRPDP.OUT])

genPL0 (AST.Call s) = do
    state <- get
    let (Just label) = Map.lookup s (env state)
    return [IRPDP.Line Nothing (IRPDP.JSR "PC" label)]

genPL0 (AST.Assign s e) = do
    e' <- genExpr e
    state <- get
    let (Just label) = Map.lookup s (env state)
    return $ e' ++ [IRPDP.Line Nothing (IRPDP.MOV label "")]

genVarsPL0 :: Maybe AST.PL0 -> State GenState ()
genVarsPL0 Nothing = return ()
genVarsPL0 (Just (AST.VarDec decs)) = mapM_ genVarPL0 decs

genVarPL0 :: AST.PL0 -> State GenState ()
genVarPL0 (AST.Var v) = do
    state <- get
    let label = IRPDP.mkLabel (sym state)
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
    let label = IRPDP.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    state <- get
    put state { env = Map.insert s label . env $ state }
    state <- get
    put state { decs = Map.insert label i . decs $ state }

genExpr :: AST.Expr -> State GenState [IRPDP.Line]
genExpr (AST.Mult l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.MUL "" "")]

genExpr (AST.Div l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.DIV "" "")]

genExpr (AST.Add l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.ADD "" "")]

genExpr (AST.LT l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.BLT "")]

genExpr (AST.LTE l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.BLE "")]

genExpr (AST.GT l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.BGT "")]

genExpr (AST.GTE l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.BGE "")]

genExpr (AST.NE l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.BNE "")]

genExpr (AST.Eq l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.BEQ "")]

genExpr (AST.Ident s) = do
    state <- get
    let (Just label) = Map.lookup s (env state)
    return [IRPDP.Line Nothing (IRPDP.MOV label "")]

genExpr (AST.IntConst i) = do
    state <- get
    let cenv = const_env state
    let const = Map.lookup i cenv
    case const of
        (Just label) -> return [IRPDP.Line Nothing (IRPDP.MOV label "")]
        Nothing      -> do let label = IRPDP.mkLabel (sym state)
                           put state { sym = succ . sym $ state,
                                       const_env = Map.insert i label (const_env state),
                                       decs = Map.insert label i (decs state) }
                           return [IRPDP.Line Nothing (IRPDP.MOV label "")]

removeNoops :: [IRPDP.Line] -> [IRPDP.Line]
removeNoops []     = []
removeNoops (x:xs) =
    if null xs
        then x:removeNoops xs
        else let (IRPDP.Line label cmd) = head xs
             in case x of
                    (IRPDP.Line l IRPDP.NOP) -> (IRPDP.Line l cmd):(removeNoops (tail xs))
                    _                   -> x:(removeNoops xs)

generateDecs :: Map.Map String Integer -> [IRPDP.Line]
generateDecs m =
    let list = Map.toList m
    in map (\(s, v) -> IRPDP.Line (Just s) (IRPDP.WORD v)) list
