unit STM;

interface

{uses ChunkTheoryUnit, classes;


type TSTM = class
              Content: Tlist;

              Procedure AddConnotation(C:TConnotation);
              Procedure RemoveConnotation(C:TConnotation);
            end;               }

implementation
{ TSTM }
{
procedure TSTM.AddConnotation(C: TConnotation);
begin
     Content.add(C);
end;

procedure TSTM.RemoveConnotation(C: TConnotation);
begin
     Content.Remove(C);
end;
}

end.
