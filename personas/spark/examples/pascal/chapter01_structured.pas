(* Structured programming in Pascal *)
function Sum(n: Integer): Integer;
var
  i, result: Integer;
begin
  result := 0;
  for i := 1 to n do
    result := result + i;
  Sum := result;
end;
