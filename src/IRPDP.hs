module IRPDP where

-- type Label = String

data Label =
    Text String     |
    Register Integer
    deriving (Eq, Ord)

instance Show Label where
    show (Text s) = s
    show (Register s) = "R" ++ (show s)

isRegister :: Label -> Bool
isRegister (Text _) = False
isRegister (Register _) = True

fromLabel :: Label -> String
fromLabel (Text s) = s
fromLabel (Register s) = show s

isEmptyLabel :: Label -> Bool
isEmptyLabel (Text s) = s == ""
isEmptyLabel (Register s) = False

data IR =
    JMP   Label         |
    MOV   Label Label   |
    JSR   Label Label   |
    OUT                 |
    ADD   Label Label   |
    SUB   Label Label   |
    DIV   Label Label   |
    MUL   Label Label   |
    CMP   Label Label   |
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
    show (JMP l)   = "JMP " ++ (show l)
    show (MOV s d) = "MOV " ++ (show s) ++ ", " ++ (show d)
    show (JSR r a) = "JSR " ++ (show r) ++ ", " ++ (show a)
    show (OUT)     = "OUT"
    show (ADD s d) = "ADD " ++ (show s) ++ ", " ++ (show d)
    show (SUB s d) = "SUB " ++ (show s) ++ ", " ++ (show d)
    show (DIV s d) = "DIV " ++ (show s) ++ ", " ++ (show d)
    show (MUL s d) = "MUL " ++ (show s) ++ ", " ++ (show d)
    show (CMP s d) = "CMP " ++ (show s) ++ ", " ++ (show d)
    show (BLT l)   = "BLT " ++ (show l)
    show (BLE l)   = "BLE " ++ (show l)
    show (BGT l)   = "BGT " ++ (show l)
    show (BGE l)   = "BGE " ++ (show l)
    show (BEQ l)   = "BEQ " ++ (show l)
    show (BNE l)   = "BNE " ++ (show l)
    show (BR  l)   = "BR " ++ (show l)
    show (HALT)    = "HALT"
    show (RTS r)   = "RTS " ++ (show r)
    show (WORD n)  = ".WORD " ++ (show n)
    show (NOP)     = "NOP"

data Line = 
    Line (Maybe Label) IR

instance Show Line where
    show (Line Nothing ir)  = "\t" ++ (show ir)
    show (Line (Just l) ir) = (show l) ++ ":\t" ++ (show ir)

mkLabel :: Integer -> Label
mkLabel = Text . ("s" ++) . show

mkRegisterLabel :: Integer -> Label
mkRegisterLabel = Register
