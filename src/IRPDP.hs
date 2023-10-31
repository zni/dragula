module IRPDP where

type Label = String

data IR =
    JMP   Label         |
    MOV   Label Label   |
    JSR   Label Label   |
    OUT                 |
    ADD   Label Label   |
    SUB   Label Label   |
    DIV   Label Label   |
    MUL   Label Label   |
    BLT   Label         |
    BLE   Label         |
    BGT   Label         |
    BGE   Label         |
    BEQ   Label         |
    BNE   Label         |
    BR    Label         |
    HALT                |
    RTS   Label         |
    WORD Integer        |
    NOP

instance Show IR where
    show (JMP l)   = "JMP " ++ l
    show (MOV s d) = "MOV " ++ s ++ ", " ++ d
    show (JSR r a) = "JSR " ++ r ++ ", " ++ a
    show (OUT)     = "OUT"
    show (ADD s d) = "ADD " ++ s ++ ", " ++ d
    show (SUB s d) = "SUB " ++ s ++ ", " ++ d
    show (DIV s d) = "DIV " ++ s ++ ", " ++ d
    show (MUL s d) = "MUL " ++ s ++ ", " ++ d
    show (BLT l)   = "BLT " ++ l
    show (BLE l)   = "BLE " ++ l
    show (BGT l)   = "BGT " ++ l
    show (BGE l)   = "BGE " ++ l
    show (BEQ l)   = "BEQ " ++ l
    show (BNE l)   = "BNE " ++ l
    show (BR  l)   = "BR " ++ l
    show (HALT)    = "HALT"
    show (RTS r)   = "RTS " ++ r
    show (WORD n)  = ".WORD " ++ (show n)
    show (NOP)     = "NOP"

data Line = 
    Line (Maybe Label) IR |
    Func (Maybe Label)

instance Show Line where
    show (Line Nothing ir)  = "\t" ++ (show ir)
    show (Line (Just l) ir) = l ++ ":\t" ++ (show ir)
    show (Func Nothing)     = "NOP"
    show (Func (Just l))    = l ++ ":"

mkLabel :: Integer -> String
mkLabel = ("s" ++) . show
