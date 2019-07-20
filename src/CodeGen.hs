module CodeGen where

import qualified AST
import qualified IR



genPL0 :: Integer -> AST.PL0 -> [IR.Line]
genPL0 n (AST.Program p) =
    genPL0 n p

genPL0 n (AST.Block c v p s) =
    let stmts  = genPL0 n s
        offset = toInteger . length $ stmts
        procs  = concat $ map (genPL0 offset) p
    in stmts ++ procs

genPL0 n (AST.Procedure s body) =
    let label = IR.mkLabel n
        n'    = succ n
    in [IR.Line (Just s) IR.NOOP] ++
       genPL0 n' body

genPL0 n (AST.Begin stmts) =
    concat $ map (genPL0 n) stmts

genPL0 n (AST.If cond stmt) =
    let label = IR.mkLabel n
        n'    = succ n
    in (genExpr cond) ++
       [IR.Line Nothing (IR.JMZ label)] ++
       (genPL0 n' stmt) ++
       [IR.Line (Just label) IR.NOOP]

genPL0 n (AST.While cond stmt) =
    let label  = IR.mkLabel n
        n'     = succ n
        label2 = IR.mkLabel n'
        n''    = succ n
    in [IR.Line (Just label2) IR.NOOP] ++
       (genExpr cond) ++
       [IR.Line Nothing (IR.JMZ label)] ++
       genPL0 n'' stmt ++
       [IR.Line Nothing (IR.JMP label2)] ++
       [IR.Line (Just label) IR.NOOP]

genPL0 n (AST.WriteLn e) =
    (genExpr e) ++
    [IR.Line Nothing IR.WRITE]

genPL0 n (AST.Call s) =
    [IR.Line Nothing (IR.JMP s)]

genPL0 n (AST.Assign s e) =
    (genExpr e) ++
    [IR.Line Nothing (IR.STORE s)]



genExpr :: AST.Expr -> [IR.Line]
genExpr (AST.Mult l r) =
    (genExpr l) ++
    (genExpr r) ++
    [IR.Line Nothing IR.MUL]

genExpr (AST.Div l r) =
    (genExpr l) ++
    (genExpr r) ++
    [IR.Line Nothing IR.DIV]

genExpr (AST.Add l r) =
    (genExpr l) ++
    (genExpr r) ++
    [IR.Line Nothing IR.ADD]

genExpr (AST.LT l r) =
    (genExpr l) ++
    (genExpr r) ++
    [IR.Line Nothing IR.LT]

genExpr (AST.LTE l r) =
    (genExpr l) ++
    (genExpr r) ++
    [IR.Line Nothing IR.LTE]

genExpr (AST.GT l r) =
    (genExpr l) ++
    (genExpr r) ++
    [IR.Line Nothing IR.GT]

genExpr (AST.GTE l r) =
    (genExpr l) ++
    (genExpr r) ++
    [IR.Line Nothing IR.GTE]

genExpr (AST.NE l r) =
    (genExpr l) ++
    (genExpr r) ++
    [IR.Line Nothing IR.NEQ]

genExpr (AST.Eq l r) =
    (genExpr l) ++
    (genExpr r) ++
    [IR.Line Nothing IR.EQ]

genExpr (AST.Ident s) =
    [IR.Line Nothing (IR.LOAD s)]

genExpr (AST.IntConst i) =
    [IR.Line Nothing (IR.LOADC i)]

