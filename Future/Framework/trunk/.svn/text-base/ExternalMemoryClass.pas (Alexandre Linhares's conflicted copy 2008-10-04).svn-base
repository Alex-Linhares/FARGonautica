unit ExternalMemoryClass;

interface

uses ChunkTheoryUnit;

type
    TExt_Mem= Class
        bricks: array[1..5] of integer;
        taken: array[1..5] of boolean;
        target, FreeBricks: integer;
        Constructor create;
    end;

    TSlipnet = Class
                  Procedure NodeCreate (N:TConnotation; IsTemplate:boolean);
                  function LinkNodes (N1, N2: TConnotation; distance: real):boolean;
    End;

    TNumboSlipnet = class (TSlipNet)

    end;

var ExtMem:TExt_Mem;

implementation

Constructor TExt_Mem.create;
begin
  bricks[1] := 5; taken[1]:=false;
  bricks[2] := 3; taken[2]:=false;
  bricks[3] := 7; taken[3]:=false;
  bricks[4] := 1; taken[4]:=false;
  bricks[5] := 2; taken[5]:=false;
  FreeBricks:=5;

  target := 49;
end;









{ TSlipnet }

function TSlipnet.LinkNodes(N1, N2: TConnotation; distance: real): boolean;
begin

end;

procedure TSlipnet.NodeCreate(N: TConnotation; IsTemplate: boolean);
begin

end;

end.
