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
    register = 0,
    sym = 0,
    env = Map.empty,
    decs = Map.empty,
    const_env = Map.empty,
    register_env = Map.empty,
    procedures = []
}

data GenState = GenState {
    register     :: Integer,
    sym          :: Integer,
    env          :: Map.Map String String,
    decs         :: Map.Map String Integer,
    const_env    :: Map.Map Integer String,
    register_env :: Map.Map Integer String,
    procedures   :: [IRPDP.Line]
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
    let l@(IRPDP.Text label) = IRPDP.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    state <- get
    put state { env = Map.insert s label . env $ state }
    body' <- genPL0 body
    return $ [IRPDP.Line (Just l) IRPDP.NOP] ++
             body' ++
             [IRPDP.Line Nothing (IRPDP.RTS (IRPDP.Register 7))]

genPL0 (AST.Begin stmts) = do
    stmts' <- mapM genPL0 stmts
    return $ concat stmts'

genPL0 (AST.If cond stmt) = do
    state <- get
    let label = IRPDP.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    state <- get
    let forwardLabel = IRPDP.mkLabel $ sym state
    put state { sym = succ . sym $ state }
    cond' <- genExpr cond
    let cond'' = reverse $ backPropagateForwardLabel forwardLabel (reverse cond')
    stmt' <- genPL0 stmt
    return (cond'' ++
            [IRPDP.Line Nothing (IRPDP.BEQ label)] ++
            [IRPDP.Line (Just forwardLabel) IRPDP.NOP] ++
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
    let addr = IRPDP.Text label
    return [IRPDP.Line Nothing (IRPDP.JSR (IRPDP.Register 7) addr)]

genPL0 (AST.Assign s e) = do
    state <- get
    reg <- mkRegister
    let (IRPDP.Register r) = reg
    let (Just label) = Map.lookup s (env state)
    e' <- genExpr e
    put state { register_env = Map.insert r label . register_env $ state }
    return $ e' ++ [IRPDP.Line Nothing (IRPDP.MOV (IRPDP.Text label) reg)]

genVarsPL0 :: Maybe AST.PL0 -> State GenState ()
genVarsPL0 Nothing = return ()
genVarsPL0 (Just (AST.VarDec decs)) = mapM_ genVarPL0 decs

genVarPL0 :: AST.PL0 -> State GenState ()
genVarPL0 (AST.Var v) = do
    state <- get
    let l@(IRPDP.Text label) = IRPDP.mkLabel (sym state)
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
    let l@(IRPDP.Text label) = IRPDP.mkLabel (sym state)
    put state { sym = succ . sym $ state }
    state <- get
    put state { env = Map.insert s label . env $ state }
    state <- get
    put state { decs = Map.insert label i . decs $ state }

genExpr :: AST.Expr -> State GenState [IRPDP.Line]
genExpr (AST.Mult l r) = do
    left <- genExpr l
    right <- genExpr r
    src <- mkRegister
    dest <- mkRegister
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.MUL src dest)]

genExpr (AST.Div l r) = do
    left <- genExpr l
    right <- genExpr r
    src <- mkRegister
    dest <- mkRegister
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.DIV src dest)]

genExpr (AST.Add l r) = do
    left <- genExpr l
    right <- genExpr r
    src <- mkRegister
    dest <- mkRegister
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.ADD src dest)]

genExpr (AST.LT l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.BLT $ IRPDP.Text "")]

genExpr (AST.LTE l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.BLE (IRPDP.Text ""))]

genExpr (AST.GT l r) = do
    left <- genExpr l
    right <- genExpr r
    let prev_src = lookupRegister left
    let prev_dest = lookupRegister right
    src <- mkRegister
    dest <- mkRegister
    if isJust prev_dest && isJust prev_src
        then return $ left ++
                right ++
                [IRPDP.Line Nothing (IRPDP.CMP (fromJust prev_src) (fromJust prev_dest))] ++
                [IRPDP.Line Nothing (IRPDP.BGT (IRPDP.Text ""))]
        else return $ left ++
                right ++
                [IRPDP.Line Nothing (IRPDP.CMP src dest)] ++
                [IRPDP.Line Nothing (IRPDP.BGT (IRPDP.Text ""))]

genExpr (AST.GTE l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.BGE (IRPDP.Text ""))]

genExpr (AST.NE l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.BNE (IRPDP.Text ""))]

genExpr (AST.Eq l r) = do
    left <- genExpr l
    right <- genExpr r
    return $ left ++
             right ++
             [IRPDP.Line Nothing (IRPDP.BEQ (IRPDP.Text ""))]

genExpr (AST.Ident s) = do
    state <- get
    let (Just label) = Map.lookup s (env state)
    dest <- mkRegister
    return [IRPDP.Line Nothing (IRPDP.MOV (IRPDP.Text label) dest)]

genExpr (AST.IntConst i) = do
    state <- get
    let cenv = const_env state
    let const = Map.lookup i cenv
    dest <- mkRegister
    case const of
        (Just label) -> return [IRPDP.Line Nothing (IRPDP.MOV (IRPDP.Text label) dest)]
        Nothing      -> do let l@(IRPDP.Text label) = IRPDP.mkLabel (sym state)
                           put state { sym = succ . sym $ state,
                                       const_env = Map.insert i label (const_env state),
                                       decs = Map.insert label i (decs state) }
                           return [IRPDP.Line Nothing (IRPDP.MOV l dest)]

removeNoops :: [IRPDP.Line] -> [IRPDP.Line]
removeNoops []     = []
removeNoops (x:xs) =
    if null xs
        then x:removeNoops xs
        else let (IRPDP.Line label cmd) = head xs
             in case x of
                    (IRPDP.Line l IRPDP.NOP) -> (IRPDP.Line l cmd):(removeNoops (tail xs))
                    _                        -> x:(removeNoops xs)

generateDecs :: Map.Map String Integer -> [IRPDP.Line]
generateDecs m =
    let list = Map.toList m
    in map (\(s, v) -> IRPDP.Line (Just (IRPDP.Text s)) (IRPDP.WORD v)) list

updateRegisters :: GenState -> State GenState ()
updateRegisters state = put state { register = succ . register $ state }

mkRegister :: State GenState IRPDP.Label
mkRegister = do
    state <- get
    let r = IRPDP.mkRegisterLabel $ register state
    updateRegisters state
    return r

lookupRegister :: [IRPDP.Line] -> Maybe IRPDP.Label
lookupRegister [] = Nothing
lookupRegister ((IRPDP.Line label ir):xs) =
    if null xs
        then extractDestRegister ir 
        else lookupRegister xs

extractDestRegister :: IRPDP.IR -> Maybe IRPDP.Label
extractDestRegister ir =
    case ir of
        (IRPDP.MOV _ dest@(IRPDP.Register d)) -> Just dest
        _                                     -> Nothing


backPropagateForwardLabel :: IRPDP.Label -> [IRPDP.Line] -> [IRPDP.Line]
backPropagateForwardLabel _ [] = []
backPropagateForwardLabel backLabel (line@(IRPDP.Line label (IRPDP.JMP op)):xs) =
    if IRPDP.isEmptyLabel op
        then (IRPDP.Line label (IRPDP.JMP backLabel)):backPropagateForwardLabel backLabel xs
        else line:backPropagateForwardLabel backLabel xs
backPropagateForwardLabel backLabel (line@(IRPDP.Line label (IRPDP.BLT op)):xs) =
    if IRPDP.isEmptyLabel op
        then (IRPDP.Line label (IRPDP.BLT backLabel)):backPropagateForwardLabel backLabel xs
        else line:backPropagateForwardLabel backLabel xs
backPropagateForwardLabel backLabel (line@(IRPDP.Line label (IRPDP.BLE op)):xs) =
    if IRPDP.isEmptyLabel op
        then (IRPDP.Line label (IRPDP.BLE backLabel)):backPropagateForwardLabel backLabel xs
        else line:backPropagateForwardLabel backLabel xs
backPropagateForwardLabel backLabel (line@(IRPDP.Line label (IRPDP.BGT op)):xs) =
    if IRPDP.isEmptyLabel op
        then (IRPDP.Line label (IRPDP.BGT backLabel)):backPropagateForwardLabel backLabel xs
        else line:backPropagateForwardLabel backLabel xs
backPropagateForwardLabel backLabel (line@(IRPDP.Line label (IRPDP.BGE op)):xs) =
    if IRPDP.isEmptyLabel op
        then (IRPDP.Line label (IRPDP.BGE backLabel)):backPropagateForwardLabel backLabel xs
        else line:backPropagateForwardLabel backLabel xs
backPropagateForwardLabel backLabel (line@(IRPDP.Line label (IRPDP.BEQ op)):xs) =
    if IRPDP.isEmptyLabel op
        then (IRPDP.Line label (IRPDP.BEQ backLabel)):backPropagateForwardLabel backLabel xs
        else line:backPropagateForwardLabel backLabel xs
backPropagateForwardLabel backLabel (line@(IRPDP.Line label (IRPDP.BNE op)):xs) =
    if IRPDP.isEmptyLabel op
        then (IRPDP.Line label (IRPDP.BNE backLabel)):backPropagateForwardLabel backLabel xs
        else line:backPropagateForwardLabel backLabel xs
backPropagateForwardLabel backLabel (line@(IRPDP.Line label (IRPDP.BR op)):xs) =
    if IRPDP.isEmptyLabel op
        then (IRPDP.Line label (IRPDP.BR backLabel)):backPropagateForwardLabel backLabel xs
        else line:backPropagateForwardLabel backLabel xs
backPropagateForwardLabel backLabel (line:xs) =
    line:backPropagateForwardLabel backLabel xs
