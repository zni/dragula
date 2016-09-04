program bool;

var
    A : Bool;
    B : Bool;
    C : Bool;

begin
    A := True;
    B := False;
    if A and B then
        C := B;
    else
        C := A;
end.
