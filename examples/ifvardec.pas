program bool;

var
    A : Bool;
    B : Bool;
    C : Bool;

begin
    A := True;
    B := False;
    if A and B then
        begin
            C := B;
            D := A
        end
    else
        C := A
end.