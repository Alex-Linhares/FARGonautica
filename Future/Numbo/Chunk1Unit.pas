unit Chunk1Unit;

interface

uses oldtrees1Unit, classes, sysutils;

type
    TOpType = (Sum, multiply, subtract, divide, OpNone);


     TChunk = class (TTree)
                   {NUMBO SPECIFIC DATA HERE}                            
                       v: integer;
                       strenght: real;
                       Salience: real;
                       Where_I_am: TObject;
                       Op:TOptype;  {Maybe this should be a class so that we could have additional behavior (such as getting a random operation),
                                    and withdrawing implementation-specific code from this unit, etc.}



                   {NUMBO SPECIFIC METHODS HERE}
                       function Operation: TOpType; virtual;
                       function value: integer; virtual;
                       function Name:string; virtual;  {returns the value in the name}
              end;

  TChunkBrick = class(TChunk)
  public
        {COMPOSITE PATTERN HERE}

        {NUMBO SPECIFIC METHODS HERE}
        constructor Create (value: integer; S: real);


  end;

  TChunkList = class(TChunk)
  public

        {NUMBO SPECIFIC METHODS HERE}
        constructor Create (S: real; OpType: TOpType);
        function Operation: TOpType; override;
        function value: integer; override;
        function Name:string;  override; {returns the value in the name}

  end;

implementation


constructor TChunkBrick.Create (value: integer; S: real);
begin
     ChunkStuff:= Tlist.Create;
     v:=value; strenght:= S;
end;

{NUMBO SPECIFIC METHODS HERE (TCHUNKLEAF WILL INHERIT THESE}
function TChunk.value: integer;
begin
     result:=v;
end;

function TCHUNK.Operation: TOpType;
begin
     result:= OpNone;
end;

Function TChunk.Name: string;
Var S:string;
begin
     str(v,S);
     Result:= S;
end;

function TCHUNKList.Operation: TOpType;
begin
     result:= Op;
end;


constructor TChunkList.Create (S: real; OpType:TOptype);
begin
     ChunkStuff:= Tlist.Create;
     strenght:= S; Op:=Optype;
end;

Function TChunkList.name: String;
var S: string; Iterator: TChunk; X:integer;
begin
     S:= '(';
     x:=Num_descendants;
     while x>0 do
     begin
          x:=x-1;
          Iterator:= Chunkstuff.items[x];
          S:=S+Iterator.Name;
          if (X>0) then
             case Operation of
                  sum: S:=S+'+';
                  multiply: S:=S+'*';
                  subtract: S:=s+'-';
             end;
     end;
     S:=S+')';
     result:=S;
end;


function TChunkList.value: integer;
var Iterator: TChunk; count, total:integer;
begin
     {RETURNS THE SIZE OF THE TREE BELOW = SIZE OF TREE-1}
     count:=0;  total:=0;
     case Operation of
          sum: Total:=0;
          subtract: Total:=0;
          multiply: Total:= 1;
     end;
     while count<Num_Descendants do
     begin
          Iterator:= Chunkstuff.items[Count];
          case Operation of
                  sum: Total:=Total+Iterator.value;
                  multiply: Total:=Total*Iterator.value;
                  subtract: begin
                                 if count<(Num_descendants-1) then Total:=Total-Iterator.value
                                 else total:=total+iterator.value;
                            end;
          end;
          Count:=Count+1;
     end;
     result:=Total;
end;

end.
