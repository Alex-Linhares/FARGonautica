unit ChunkProject;

interface

uses
  Node1Unit, Numboconnotations,  NumboSlipnet1Unit,
  ExternalMemoryClass, FARG_Framework_Chunk,  FrameworkConnotationInfo,
  NumboViewer, Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls;

type              
  TForm1 = class(TForm)
    TabControl1: TTabControl;
    Button1: TButton;               
    Memo1: TMemo;                   
    Button3: TButton;
    Button4: TButton;
    Button2: TButton;
    Button5: TButton;
    Button6: TButton;

    procedure Button1Click(Sender: TObject);           
    Procedure Look_at_target;
    Procedure Look_at_Brick;
    procedure FormCreate(Sender: TObject);
    Function PrintConnotationString(C:TConnotation):String;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);

    //in THIS class?
    Procedure TryBottomUp;
    Procedure TryTopDown(H:Toperations);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    Function GetMeaningOfConnotation(C: TConnotation):TList;

    public
    CH1: TChunk;
    function HypothesisTest(R:TOperations; Expect:Tlist):TList; overload;

    Function PrintTheAssociations(N:KernelNode):TList;
    Function PrintExploded(C:TConnotation):TList;
    Procedure PrintConnotations(L:TList);

    Procedure STM_Init;

  end;

var Form1: TForm1;
    Brick: Tbrick;
    Target: TTarget;
    Subgoal:Tnumberinteger;
    NumboSlipnet: TNumboSlipnet;


implementation
{$R *.dfm}


Function GetAvailableSTMItems:TList;
var x: integer; L:TList; C:TConnotation;
begin
    L:=TList.Create;
    for x := 0 to STM.Count - 1 do
    begin
       C:=STM[x];
       if C.InheritsFrom(TBrick) then
          L.add(C);
    end;
    result:=L;
end;




Procedure PrintJunkMethod (ReceivedConnotation:TConnotation);
var AvailableSTMItems, NeededItems: TList;
    C, c2:TConnotation;
    i, j, NumNeeded, NumAvailable:integer;
    res:real;

begin
    AvailableSTMItems:=GetAvailableSTMItems;
    NeededItems:=TList.Create;
    NeededItems:=ReceivedConnotation.GetBasicElements(NeededItems);
    //NeededItems does not have Tresults (Or operations for that matter)


    if (AvailableSTMItems.count*NeededItems.count<>0) then
    begin
         Form1.Memo1.Lines.Add(' needed:');
         Form1.PrintConnotations(NeededItems);

         Form1.Memo1.Lines.Add(' available:');
         Form1.PrintConnotations(AvailableSTMItems);
    end else exit;
end;




Function STMAvailability (ReceivedConnotation:TConnotation):real;
var AvailableSTMItems, NeededItems: TList;
    C, c2:TConnotation;
    i, j, NumNeeded, NumAvailable, TotalNeeded:integer;
    res:real;

begin
    AvailableSTMItems:=GetAvailableSTMItems;
    NeededItems:=TList.Create;
    NeededItems:=ReceivedConnotation.GetBasicElements(NeededItems);
    //NeededItems does not have Tresults (Or operations for that matter)

    NumAvailable:=0;
    NumNeeded:=0;
    NumNeeded:=NeededItems.Count;
    TotalNeeded:=NumNeeded;

    i:=0;
    while (i<= NeededItems.Count-1) do
    begin
        C:=NeededItems[i];
        begin
            j:=0;
            while j<= AvailableSTMItems.Count - 1 do
            begin
                C2:=AvailableSTMItems[j];
                if C2.InheritsFrom(TNumberInteger) then
                  if TNumberInteger(C).Value=TNumberInteger(C2).Value then
                  begin
                      NumAvailable:=NumAvailable+1;
                      AvailableSTMItems.Remove(C2);

                      //we need to delete both elements now,
                      //because another one with the same value might be needed
                      //seems that remove removes them all, so trying delete...
                      NeededItems.remove(C);

                      i:=i-1;
                      break;
                  end;
                  j:=j+1;
            end;
        end;
        i:=i+1;
    end;

    if TotalNeeded=0 then res:=0
    //else if NumAvailable>=NumNeeded then res:=1
    else res:=(NumAvailable)/TotalNeeded;
     {bug1 is here}
    Result:=res;
end;









function ListSortCompare (Item1, Item2: Pointer): Integer;
var MyObject1, MyObject2: TConnotation;
begin
  MyObject1 := TConnotation(Item1);
  MyObject2 := TConnotation(Item2);
  if MyObject1.Relevance(Target, STMAvailability(MyObject1)) < MyObject2.Relevance(Target,STMAvailability(MyObject2))
  then Result := 1
  else if MyObject1.Relevance(Target, STMAvailability(MyObject1)) > MyObject2.Relevance(Target, STMAvailability(MyObject2))
  then Result := -1
  else Result := 0; // equal                          
end;



procedure TForm1.Look_at_target;
var s:string;
begin
    {loads target into Target/TNumber/tProperty}
    {how do you do this?}
    {external memory has an int; bottomup scout of target must grab it and fill all needed info}
    Target.BottomUpPropose;

    if (STM.IndexOf(Target)<0) then
    begin
      {get full name?}
      with target do
      str(GetValue,s);     {Duplicate code (look above)}
      memo1.Lines.Add(Target.ClassName+' is '+s);
      STM.Add(target);
    end;

    //NumboSlipnet.ExplodeNumber(Target.GetValue);
    //PrintTheAssociations(NumboSlipnet.GetSlipnetNodeWithValue(target.getvalue));
end;



procedure TForm1.FormCreate(Sender: TObject);
var x:integer;
s:string;
begin
    ExtMem:=TExt_mem.Create;
    Target:=TTarget.Create;
    STM:= TList.Create;
    STM_Init;

    NumboSlipnet:=TNumboSlipnet.Create;

    RandSeed:=-1165225810;
    randomize;

    x:=RandSeed;
    str(x,S);
    Memo1.Lines.add(s);
end;



procedure TForm1.Look_at_Brick;
var s:string;
begin
    Brick:=TBrick.Create;

    while (Brick.Value=0) do brick.BottomUpPropose;

    if (STM.IndexOf(Brick)<0) then
    begin
      with Brick do
      str(GetValue,s);
      memo1.Lines.Add(Brick.ClassName+' is '+s);
      STM.Add(Brick);
    end;
end;



procedure TForm1.STM_Init;
var s:string;
begin
    {loads bricks and target and displays them}
    Look_at_Target;
    
    while extmem.FreeBricks>0 do
        Look_at_Brick;

    {just checking that there are no duplicate bricks by any chance}
    str (STM.Count, s);
    memo1.lines.Add('elements in STM:'+s);
end; 






procedure TForm1.TryBottomUp;
var R:TOperations;
begin
    //bottom-up part creates numerous multiplications
    R:=TOperations.GetRandomOperation;  //factory method generates random operation
    R.BottomUpPropose;              //we are creating a relation, not a chunk
    R.CommitToSTM;                  //And here's a commit
    Memo1.Lines.Add(PrintConnotationString(r));
end;


Function DeleteRepeatedElementsOfList(L:TList):TList;
var x, y:integer;
begin
    x:=0;
    While X<(L.Count-1) do
    begin
      y:=x+1;
      While Y<L.Count do
      begin
          if (L.Items[x]=L.Items[y]) then
          begin
            L.Remove(L.Items[y]);
            y:=y-1;
          end;
          y:=y+1;
      end;
      x:=x+1;
    end;
    result:=L;
end;


                  

Procedure Tform1.TryTopDown(H:Toperations);
var R:TOperations;
begin
    //top-down now, from "node" H created above...
    R:=H.ProposeHypothesis;
    If (R<>nil) then //if found on STM, then...
        Memo1.Lines.add('................Searching for:'+PrintConnotationString(R));
end;











//REFACTOR!! VERY UGLY, Repetitive code, just kill this soon.
function TForm1.HypothesisTest(R:TOperations; Expect:Tlist):TList;
var
    L, FoundInSTM, NotFound, InterestingThings, using:TList;
    C:TConnotation;
    STM_N: TNumberInteger;
    I, j: Integer;
    Node: KernelNode;

begin
    L:=R.GetAssociationsOfType(TNumberInteger);
    FoundInSTM:=R.ExpectationsFoundInSTM(L);
    memo1.Lines.add('Gonna use this '+PrintConnotationString(TNumberInteger(FoundInSTM.items[0])) +' here');
    NotFound:=TList.create;
    NotFound.assign(FoundInSTM, laDestUnique, L);
    DeleteRepeatedElementsOfList(NotFound);


    //What do we lack?
    for I := 0 to NotFound.Count - 1 do
    begin
      C:=NotFound.items[i];
      memo1.Lines.add('need a '+PrintConnotationString(C));

      //we must explode connotations of this guy
      NumboSlipnet.explodeNumber(TNumberInteger(C).GetValue);
      Node:=NumboSlipnet.GetSlipnetNodeWithValue(TNumberInteger(C).GetValue);
      //PrintAssociations(Node);

      //How to get the right set of NewExpectations?
      InterestingThings:=NumboSlipnet.GetNodesWhichResultIn(TNumberInteger(C).GetValue);

      for j := 0 to InterestingThings.Count - 1 do
      begin
          Expect.add(InterestingThings.items[j]);
          memo1.Lines.add('  hmm... maybe this is interesting: '+PrintConnotationString(InterestingThings.items[j]));
      end;

      //we need to change expections here, somehow...
      //Expectations:= ????

    end;
    //Result:=Expectations;
end;






//Plays around with top-down and bottom-up stuff
procedure TForm1.Button2Click(Sender: TObject);
var i, j:integer;
    N:KernelNode;
    C, c2:TConnotation;
    Desirable, Desirable2:TList;
    S, s2:String;
    Ordered:TList;
//    Comp: TListSortCompare;

begin
    STM_Init;
    C:=target;
    Ordered:=TList.Create;

    Desirable:=NumboSlipnet.ExplodeConnotations(TNumberInteger(C));
    Ordered.add(c);
    for I := 0 to Desirable.Count - 1 do
    begin
      C:=Desirable.items[i];
      Ordered.add(c);

      if C.inheritsfrom(TNumberInteger) then
          Desirable2:=NumboSlipnet.ExplodeConnotations(TNumberInteger(C));
      for j := 0 to Desirable2.Count - 1 do
      begin
          C2:=Desirable2.items[j];
          Ordered.Add(c2);
      end;
    end;

    Ordered.Sort(ListSortCompare);
    for I := 0 to Ordered.Count - 1 do
      memo1.Lines.add(PrintConnotationString(TConnotation(Ordered[i])));
end;

procedure TForm1.Button3Click(Sender: TObject);
var H:TOperations;
  x: Integer;
  s:string;
  Expect:TList;
  Node:KernelNode;

begin
    FORMCREATE(SELF);
    STM_Init;                                                                     
    {just checking that there are no duplicate bricks by any chance}
    str (STM.Count, s);
    memo1.lines.Add('elements in STM:'+s);

    Node:=NumboSlipnet.GetSlipnetNodeWithValue(target.GetValue);
    Expect:=Node.GetAssociationsOfType(TOperations);
    //so there's a loop in here, in which bottom-up and top-down compete
    for x := 0 to 10 do
    begin
         //top-down now, from "node" H created above...
         H:=Expect.items[Random(Expect.count)];
         Memo1.Lines.Add('let`s see if we can find '+PrintConnotationString(H));
         Expectations:=HypothesisTest(H,Expect);

         //bottom-up part creates random operation
         //TryBottomUp;
    end;
end;


Function  TForm1.PrintTheAssociations(N:KernelNode):TList;
var L:TList; x:integer; C:TConnotation;
begin
    memo1.Lines.Add('for node '+PrintConnotationString(N.Content));
    L:=N.GetAssociationsOfType(TConnotation);
    for x := 0 to L.Count - 1 do
    begin
        C:=L.items[x];
        memo1.Lines.add(PrintConnotationString(C));
    end;
    result:=L;
end;


Procedure TForm1.PrintConnotations(L:TList);
Var j:integer; C: TConnotation;
begin
    for j := 0 to L.Count - 1 do
    begin
        C:=L[j];
        memo1.Lines.Add(PrintConnotationString(C));
    end;
end;



Function TForm1.PrintExploded(C:TConnotation):TList;
Var Desirable: TList; j:integer; C2: TConnotation;
begin
    Desirable:=NumboSlipnet.ExplodeConnotations(C);
    for j := 0 to Desirable.Count - 1 do
    begin
        C2:=Desirable[j];
        memo1.Lines.Add(PrintConnotationString(C2));
    end;
    Result:=Desirable;
end;











//Button tests some top-down connotation explosions
procedure TForm1.Button4Click(Sender: TObject);
var i, j:integer;
    N:KernelNode;
    C, c2:TConnotation;
    Desirable, Desirable2:TList;
    S, s2:String;
    Ordered:TList;

begin
    STM_Init;
    Memo1.lines.add('===Nodes in the NumboSlipnet');
    For I:= 0 to NumboSlipnet.nodes.count-1 do
    begin
        N:=NumboSlipnet.nodes.items[i];
        Memo1.Lines.Add(PrintConnotationString(N.Content));
    end;

    {memo1.Lines.Add('===desirable');
    C:=target;
    Ordered:=TList.Create;

    Desirable:=NumboSlipnet.ExplodeConnotations(C);
    Ordered.add(c);
    memo1.Lines.Add('for '+PrintConnotationString(C));
    for I := 0 to Desirable.Count - 1 do
    begin
      C:=Desirable.items[i];
      memo1.Lines.Add('we have: '+PrintConnotationString(C));
      Ordered.add(c);

      Desirable2:=NumboSlipnet.ExplodeConnotations(C);
      for j := 0 to Desirable2.Count - 1 do
      begin
          C2:=Desirable2.items[j];
          memo1.Lines.Add('...or also: '+PrintConnotationString(C2));
          Ordered.Add(c2);
      end;
    end;
    s:='';
    NumboSlipnet.ExplodeConnotations(C2);
    PrintExploded(C2);

    Ordered.Sort(ListSortCompare);
    for I := 0 to Ordered.Count - 1 do
      memo1.Lines.add(PrintConnotationString(TConnotation(Ordered[i])));
    }
end;








//Each node in desirable needs to have a path to the target
Function CheckForLinksToTarget(Desirable:TList):TList;
var
  I, r: Integer;
  C:TConnotation;
  Op:TOperations;
  Number:TNumberInteger;
  N, TargetN: KernelNode;
begin
    i:=0;
    while (i<=Desirable.Count-1) do
    begin
        //how do we know if there is a path to the target?
        C:= Desirable[i];
        if C.InheritsFrom(TOperations) then
        begin
            r:=TOperations(C).GetMyResult;
            N:= NumboSlipnet.GetSlipnetNodeWithValue(r);

            if N<>nil then
            begin
                C:=N.Content;
                Desirable.add(C);
                i:=i+1;
            end;
        end;

        If C.InheritsFrom(TAtribute) then
        begin
            Desirable.Remove(C);
            i:=i-1;
            N:=NumboSlipnet.GetSlipnetNodeWithValue(TNumberInteger(C).GetValue);
            TargetN:=NumboSlipnet.GetSlipnetNodeWithValue(Target.Value);
            //How to create a path to the target?
            //in NUMBO is easy, just get the target and the appropriate relation in question
            Number:=TNumberInteger(C);
            if Target.GetValue<>Number.GetValue then
            begin
                if target.Value<Number.Value then
                    Op:=NumboSlipnet.GetNewOperation(Number.Value, (Number.Value-Target.Value), TSubtraction)
                else if target.Value>Number.Value then
                    Op:=NumboSlipnet.GetNewOperation(Number.Value, (Target.Value- Number.Value), TAddition);

                Op.ComputeRelation;
                NumboSlipnet.CreateNode(Op);
                if Target.Value<Number.Value then
                    NumboSlipnet.LinkNodes(TargetN, NumboSlipnet.GetNode(Op))
                else NumboSlipnet.LinkNodes(NumboSlipnet.GetNode(Op), TargetN);
                Desirable.Add(Op);
            end;
        end;
        i:=i+1;
    end;
    result:=Desirable;
end;

Function RemoveThoseLessRelevantThan(L:TList; R:real):TList;   //mediator between connotations & stm
var i:integer; C:TConnotation;
begin
    i:=0;
    while i<=L.Count-1 do
    begin
        C:=L[i];
        if (C.Relevance(Target, STMAvailability(C))<=R) then
        begin
            L.Remove(C);
            i:=i-1;
        end;
        i:=i+1;
    end;
    result:=L;
end;


//RUNs NUMBO... in a top-down, 2 level depth-first search
procedure TForm1.Button5Click(Sender: TObject);
var rounds, x:integer;
    C, c2, c3:TConnotation;
    Desirable, Desirable2, Desirable3, NeededItems:TList;
    S:String;
    I, J, k: Integer;
    cutpoint:real;

begin
    formcreate(self);

    memo1.Lines.Add('=============================================================');
    C:=target;
    memo1.Lines.Add('FOR TARGET '+PrintConnotationString(C));

    Desirable:=GetMeaningOfConnotation (C);
    Desirable:= RemoveThoseLessRelevantThan(Desirable, -0.1);

    PrintConnotations(Desirable);
    cutpoint:=0.000001;

    for I := 0 to Desirable.Count - 1 do            //for each original theory...
    begin
        C:=Desirable[i];
        memo1.Lines.add('');
        memo1.Lines.add('');

        memo1.Lines.add('ORIGINAL THEORY IS ****************************'+PrintConnotationString(C));
        PrintJUNKMethod(C);
        Desirable2:=GetMeaningOfConnotation(C);

        Cutpoint:=C.Relevance(Target, STMAvailability(C));
        Desirable2:= RemoveThoseLessRelevantThan(Desirable2, {0.1}cutpoint);



        for j := 0 to Desirable2.Count-1 do
        begin
            C:=Desirable2[J];
            str(j, s);
            PrintConnotations(Desirable2);
            memo1.Lines.add('...............................................(1) Round '+s+'-->'+PrintConnotationString(C));
            memo1.lines.add(' needing...');
            NeededItems:=tlist.create;
            NeededItems:=C.GetBasicElements(NeededItems);
            PrintConnotations(NeededItems);

            Cutpoint:=C.Relevance(Target, STMAvailability(C));

            Desirable3:=GetMeaningOfConnotation(C);
            Desirable3:= RemoveThoseLessRelevantThan(Desirable3, {0.1}cutpoint);

            for k := 0 to Desirable3.Count-1 do
            begin
                C:=Desirable3[k];
                str(k, s);
                PrintConnotations(Desirable3);
                memo1.Lines.add('.....................................................(2) Round '+s+'-->'+PrintConnotationString(C));
            end;
        end;
    end;
    memo1.lines.add('');
    Desirable2.Destroy;
end;



//Parallel terraced scan: think about area-restricted search
//Temperature = Tolerance towards Relevance = Max distance from original point
//Neurons will appear here
procedure TForm1.Button6Click(Sender: TObject);
var x, Level, LevelThreshold, Effort, EffortThreshold:integer;
    Tolerance: array[0..30] of real;
    searching:boolean;
    CurrentIdea, BestIdea:TConnotation;
    SubCog: TList;
    delta, best:real;

begin
    formcreate(self);
    Searching:=true;
    x:=0;
    tolerance[3]:=0;
    level:=0;
    LevelThreshold:=5;
    EffortThreshold:=50;
    Tolerance[0]:=0;                        
    SubCog:=TLIst.Create;
    delta:=0;
    best:=-0.01;

    CurrentIdea:=target;
    memo1.Lines.Add('FOR TARGET '+PrintConnotationString(CurrentIdea));

    While Searching do
    begin
        SubCog:=GetMeaningOfConnotation (CurrentIdea);
        SubCog:= RemoveThoseLessRelevantThan(SubCog, best);
        if subcog.Count=0 then exit;

        delta:=( CurrentIdea.Relevance(Target, STMAvailability(CurrentIdea)) - TConnotation(SubCog[0]).Relevance(Target, STMAvailability(TConnotation(SubCog[0]))) );
        if delta < 0 then
        begin
            //accept;
            CurrentIdea:=SubCog[0];
            if CurrentIdea.Relevance(Target, STMAvailability(CurrentIdea))>Best then
            begin
                Best:=CurrentIdea.Relevance(Target, STMAvailability(CurrentIdea));
                BestIdea:=CurrentIdea;
                Memo1.lines.add( PrintConnotationString(BestIdea));
                effort:=0;
            end;
        end;                         

        //reject;
        if effort>effortThreshold then
            Searching:=false;

        effort:=effort+1;
        CurrentIdea:=SubCog[{random(subcog.count)}0];
        //Tolerance[level]:=CurrentIdea.Relevance(Target, STMAvailability(CurrentIdea));
    end;
end;



Function TForm1.GetMeaningOfConnotation(C: TConnotation):TList;
var Meaning: TList;
begin
  Meaning := TList.Create;
  Meaning := NumboSlipnet.ExplodeConnotations(C);
  Meaning := CheckForLinksToTarget(Meaning);
  Meaning.Sort(ListSortCompare);
  result:=Meaning;
end;


Function TForm1.PrintConnotationString(C:TConnotation):String;
var L2:TList; S1: TStringName;S2 :TString; v, SValue:string;
begin
    L2:=TList.Create;
    S1:=TStringName.Create;
    S1.Elements.Add(C);
    S1.ComputeRelation;
    L2:=S1.NewElements;
    S2:=L2.items[0];
    str (C.Relevance(Target, STMAvailability(C)):0:3, v);

    if C.InheritsFrom(TNumberInteger) then
      str ((TNumberInteger(C).GetValue), SValue);

    if C.InheritsFrom(TOperations) then
      str (TOperations(C).GetValue, SValue);

    SValue:='      results in '+SValue;
    result:=S2.GetValue+' ('+v+')'+SValue;
end;



//This button generates operations in a bottom-up manner
procedure TForm1.Button1Click(Sender: TObject);
var s:string;
    C, C2: TConnotation;
    Chunk:TNumboChunk;
    I, I2: Integer;
    ChunkInfo: TConnotationInfo;
    L:Tlist;

begin
    formCreate(self);
    STM_Init;

    while (STM.count>2) do
    begin
      {now testing TMultiplication.GetBottomUpConnotationTypes}
      memo1.Lines.Add('-----------------------------------------------' );
      TryBottomUp;
      memo1.Lines.Add('-----------------------------------------------' );

      //scans STM, looking for a multiplication, and chunks it
      Chunk:=TNumbochunk.Create;
      Chunk.BottomUpPropose;
      Chunk.CommitToSTM;

      {now we should find a way to print out chunks here}
      I:=0;
      While (i<STM.Count) do
      begin
           C:=STM.items[i];
           Memo1.Lines.Add(PrintConnotationString(C));
           if (C.inheritsfrom(TChunk)) then {duplicate code 1 again}
           begin
              ChunkInfo:=TConnotationInfo.create(TNumboChunk(C));
              str (ChunkInfo.GetChunkDepth(TNumboChunk(C)), S);
              Memo1.Lines.Add('Depth of Chunk='+S );

              {displays classes involved in chunk, created now and previously}
              L:=TList.Create;  s:='';
              L:=TChunk(C).ListAllConnotations(L);
              for i2 := 0 to L.Count - 1 do
              begin
                  C2:=L.Items[i2];
                  s:=s+C2.ClassName+' ';
              end;
              memo1.Lines.Add(S);
          end;
          i:=i+1;
      end;
    end;
end;


end.