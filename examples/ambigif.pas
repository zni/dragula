program ambiguous;

var
    A : Bool;
    B : Bool;
    C : Bool;

begin
    A := True;
    B := False;
    C := True;
    if A and B then
        if A or C then
            C := B
        else
            C := A
end.

{
If (And (Ident "A") (Ident "B"))
    (If (Or (Ident "A") (Ident "C"))
        (Assign "C" (Ident "B"))
        (Just (Assign "C" (Ident "A"))))
    Nothing
}
