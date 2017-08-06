unit WorkingMemory1Unit;

interface

uses Chunk1Unit, OldTrees1Unit;

Type TWorkingMemory = class
                          WM: TChunk;
                          constructor create (C: Tchunk);
                          procedure Random_Chunk_Mover;
                          procedure Random_ChunkList_Includer;
                          procedure Random_Chunk_Breaking;                
                          Procedure Chunk_Going_Up;
                          Procedure Random_Block_Creator;
                     end;

implementation

constructor TWorkingMemory.create (C: Tchunk);
begin
     WM:= C;
     {The Working memory consists of a chunk for implementation purposes only}
     {so recursion is obtained easily}
     {Here we receive as a parameter, isolated pieces in a chunk C (all at the same level)}
end;

procedure TWorkingMemory.Random_Chunk_Mover;
var Chunk: Tchunk; Chunk1, Chunk2: TTree; Op: TOpType; x:integer;
begin

     x:=Wm.Get_random_Position;
     Chunk1:=WM.Get_Item_in_Position(x);
     if (chunk1=nil) then Wm.InsertChunk_in_Position(Chunk1,x)
     else begin
               WM.Remove(Chunk1); {once removed, never to return-->THIS IS THE BUG!!!!!!!!!!!}

               x:=Wm.Get_random_Position;
               Chunk2:=WM.Get_Item_in_Position(x);
               if (chunk2=nil) then Wm.InsertChunk_in_Position(Chunk2,x)
               else begin
                         WM.Remove(Chunk2);
                         {Create some operation}
                         Op:= multiply;
                         Chunk:=TChunkList.Create(1.0, Op);
                         Chunk.add(chunk1,0);
                         Chunk.add(chunk2,0);

                         WM.InsertChunk_in_Position(Chunk, Wm.Get_random_Position);
                   end;
          end;
end;

procedure TWorkingMemory.Random_Block_Creator;
var Chunk: TTree;
begin
     {select a random folk from inside a chunk (i.e., not level 0)}

     Chunk:=WM.Get_Item_in_Position(WM.Get_Random_Position);
     WM.Remove(Chunk);
     Wm.InsertChunk_in_Position(Chunk, wm.Get_Random_Position);
end;

procedure TWorkingMemory.Random_ChunkList_Includer;
var Chunk: TChunk; Op: TOpType;
begin
     {select operation / rule / how pieces fit together / how things relate}

     {DON'T CODE TO AN IMPLEMENTATION, U MORON!}
     Op:= multiply;
     Chunk:=TChunkList.Create(1.0, Op);
     {traverse the chunk and include "Chunk" in position y}
     WM.InsertChunk_in_Position(Chunk, wm.Get_Random_Insert_Position);
end;

procedure TWorkingMemory.Random_Chunk_Breaking;
var Chunk: TTree;
begin
     {select a random folk from inside a chunk (i.e., not level 0)}
     Chunk:=WM.Get_Item_in_Position(wm.Get_Random_Position);
     WM.Remove(Chunk);
end;



Procedure TWorkingMemory.Chunk_Going_Up;
begin
end;



end.
