unit ChunkTheoryUnit;

interface

 uses classes;

type
  TChunk = class;             

  EProposalState = (Propose, CheckStrength, CommitToMemory);  {"Expect" for top-down, "register" synonym for bottom-up?}

  TConnotation = class (TinterfacedObject)     
                    ExtMemoryRef: TObject;
                    ExpectedConnotation, ExpectedConnotationFound: TConnotation;
                    Relevance: real;
                    State: EProposalState;

                    Function Contains (ConnotationType:TClass):boolean; virtual;
                    Procedure Codelet; {Change to state pattern and template pattern later on}

                    {== codelets, MM style}
                    Function TopDownSeekFor(C: TConnotation):TConnotation;   virtual;
                    Procedure BottomUpPropose; virtual;
                    Procedure Test; virtual; abstract;
                    Procedure CommitToSTM; virtual; abstract;

                    {supporting actors}
                    Procedure SearchForInstance; virtual; abstract;
                    function StringName: string; virtual; abstract;
                    function Print:string; virtual; abstract;
                end;

  TProperty = class (TConnotation)
                 {incognita? UndefinedValue:boolean???}
                 Function GetValue: TObject; virtual; abstract;
                 Function ExactValueCheck(N:TProperty):boolean; virtual; abstract;
                 Function TopDownSeekFor(C: TConnotation):TConnotation; override;
              end;

  TRelation = class (TConnotation)
                  {what is a relation?
                   each relation has some items, each item has a role
                   items with roles...

                   AND a function

                   A relation finds items of a certain kind (their roles),
                   and creates a new item of a certain kind, maybe even with a particular value

                   NUMBO:
                   Multiplication: item1 (operand) item2 (operand) item3 (result)

                   COPYCAT:
                   SUCESSOR: item1 (letter_value) item2 (Letter_Value) Alphabetic_Distance(Item1,Item2)=1 (number);

                   CHESS:
                   Attack: item1(piece) item2(piece) move_distance(item1,item2)=1(number) (attacker in item1) (attacked in item2)
                  }
                  OriginalElements: tlist;
                  NewElements: tlist;
                  AcceptableConnotations:TList;

                  Constructor create;
                  Procedure BuildItUp; virtual;
                  Function StringName:string; override;

                  Procedure SearchForInstance; override;
                  Procedure GetOriginalElements; virtual;
                  Function ConnotationIsAcceptable(C:TConnotation):boolean; virtual;
                  Function MakeRelationChunk(RelationItems:Tlist):TChunk; virtual;

                  {
                  abstract methods=template pattern
                  these do not affect the class structure interface
                  }
                  Procedure GetAcceptableConnotationsTypes; virtual; abstract;
                  Function ComputeRelation (RelatedItems: TList):TList; virtual; abstract;
                  Function GetRelatedItems:TList; virtual; abstract;
                  Function ConditionsAreSatisfied: boolean; virtual; abstract;
              end;

  TChunk  = class (TConnotation)
              ConnotationsAtThisLevel: Tlist;
              AllChunkConnotations: Tlist;

              Constructor Create;
             { Procedure BuildItUp; virtual;}

              Procedure ChunkRelation(R:TRelation);
              Procedure AcquireConnotations (L: TList);
              Procedure CommitToSTM;  override;
              Function Print:string; override;
              Function StringName:string; override;
              Function Contains (ConnotationType:TClass):boolean; override;
              function GetConnotationOfType(T: TClass): TConnotation;
              Function TopDownSeekFor(C: TConnotation):TConnotation; override;
              Procedure Destroyer;
              Function GetRelationsThatBindChunk: TList;
              Function GetConnotationsFromTheseRelations(Relations:TList):TList;
            end;

{===========================================}

  TSameThing = class
                  private
                  C1, C2: TChunk;
                  res: boolean;
                  Connotation1, Connotation2: TConnotation;
                  index:integer;
                  public
                  function Compare(Chunk1, Chunk2:TChunk):Boolean;
                  procedure ComparisonSubroutine; virtual; abstract;
  end;

  TChunks = class (TSameThing)
                  procedure ComparisonSubroutine; override;
  end;

  TTemplates = class (TSameThing)
                  procedure ComparisonSubroutine; override;
  end;

  TSet = class (TSameThing)
                  procedure ComparisonSubroutine; override;
  end;

var     STM_Content: TList;

implementation

{ TConnotation }

procedure TConnotation.Codelet;
{THIS SHOULD BE REFACTORED TO STATE PATTERN SOMEDAY}
begin
     if State=Propose then
     begin
          if ExpectedConnotation<>nil then
            TopDownSeekFor(ExpectedConnotation) else
            BottomUpPropose;
     end else
     if State=CheckStrength then
        Test else
     if State=CommitToMemory then
        CommitToSTM;
end;

Procedure TConnotation.BottomUpPropose;
begin
  SearchForInstance;
end;

function TConnotation.Contains(ConnotationType: TClass): boolean;
begin
    if self.ClassType<>ConnotationType then
      result:= false else result:=true;
end;

Function TConnotation.TopDownSeekFor(C: TConnotation):TConnotation;
begin
    {This method looks for a sentconnotation and creates a scout (proposal)}
    {one connotation is being searched... it could have a value,
     or it could have an UndefinedValue (only the type is being searched for)}
    {How do we know if the connotation is there? We must look into STM}
    {Is there a difference between finding a specific value OR only a type?}

    {How to check if two objects are equal?  Let's say we're looking for a number
    in Numbo... then that's easy: check the value; but what if we're looking for
    a 100+10=110 chunk? How to compare whole structures? Maybe AllChunkConnotations
    can be used?  this has many levels and many connotations and values,
    which make the test much harder...}

    {examples of method in action, different hierarchical levels:

    NUMBO:    looks for a particular number or result
              looks whether a particular number can result from some operation
              Jarbas system; looks for a particular type of operation? x+y=z ?

    Copycat:  looks for a successorship_relation between any two letters
              looks for a chunk of a certain type (sameness, successorship, etc)

    Chess:    looks for upcoming moves of a certain piece
              looks for interceptions of certain relations

    Bongard Problems: looks for certain types of geometrical objects
                      looks for certain types of relations between objects

    So,
    (i) we have a certain connotation which is expected to be found in STM or external memory
    (ii) With this SentConnotation, we browse memory systems in search of it...
    (iii) How do we browse memory systems?
      (a) each ExpectedConnotation can only be found in a certain types of conotations
          (e.g., it is useless to browse a tbrick for a multiplication, or a letter for
           a sucessorship group, or a chess queen for an attack, or a triangle for a bigger-than
           relation)
           So, for each ExpectedConnotation, we should define a method like
           ToScan:=CanBeFoundIn(ExpectedConnotations):Tlist;
      (b) after we have a set of things ToScan, we should, perhaps, apply this:
           C:=ToScan.Items[x];  (C=random item of a good connotation type)
           C.SearchForInstance; (This method is type-only, value neutral... so
                                 there should be another method in which the VALUE
                                 is being searched for.)

                                 BRAINSTORM: How does this compare to the idea of an incognita???

      So what is the difference between bottom-up and top-down codelets?

      bottom up just SearchForInstance
      top-down have to scan a list of good connotations???  IN STM alone?  Or in EM also?
    }

   {TCHUNK calls this for each internal connotation}
   ExpectedConnotation:=C;
   ExpectedConnotationFound:=nil;
   if ExpectedConnotation.ClassType=Self.ClassType then   {Checking exact classes here}
   begin
      ExpectedConnotationFound:=self;
   end;
   result:=ExpectedConnotationFound;
end;


{ TChunk }

Function TChunk.TopDownSeekFor(C: TConnotation):TConnotation;
var
  I: Integer;
  Aux: TConnotation;
begin
  ExpectedConnotation:=C;
  {looks for the conotation inside the chunk (but should also look if the chunk is a perfect match)}
  if (ExpectedConnotationFound<>self) then
  begin
    for I := 0 to ConnotationsAtThisLevel.Count - 1 do
    begin
      aux:=ConnotationsAtThisLevel.Items[i];
      ExpectedConnotationFound := aux.TopDownSeekFor (ExpectedConnotation);
    end;
  end;
  result:=ExpectedConnotationFound;
end;

procedure TChunk.CommitToSTM;
var
  I: Integer;
  C: TConnotation;
begin
  for I := 0 to ConnotationsAtThisLevel.Count - 1 do
  begin
    C:= ConnotationsAtThisLevel.items[I];
    if (STM_Content.IndexOf(C)>=0) then
    begin
        STM_Content.Remove(C);
        STM_Content.Pack;
    end;
  end;
  STM_Content.Add(self);
end;

function TChunk.Contains(ConnotationType: TClass): boolean;
var C: TConnotation;
I: integer;
res:boolean;
begin
    res:=false;  
    for I := 0 to AllChunkConnotations.Count - 1 do
    begin
       C:=AllChunkConnotations.Items[i];
       if C.ClassType=ConnotationType then
          res:= true;
    end;
    result:=res;
end;

constructor TChunk.Create;
begin
    inherited;
    AllChunkConnotations:=Tlist.Create;
    ConnotationsAtThisLevel:= Tlist.Create;
end;

procedure TChunk.AcquireConnotations(L: TList);
var
  I, I2: Integer;
  C, C2: TConnotation;
begin
     for I := 0 to L.Count - 1 do
     begin
       C:=L.Items[i];
       ConnotationsAtThisLevel.add(C);
       {the whole structure, including subchunks}
       AllChunkConnotations.Add(C);

       if C.ClassType=TChunk then
       begin
          for I2 := 0 to TChunk(C).AllChunkConnotations.Count - 1 do
          begin
            C2:=TChunk(C).AllChunkConnotations.Items[i];
            {the whole structure, including subchunks}
            AllChunkConnotations.Add(C2);
          end;
       end;
     end;
end;


procedure TChunk.Destroyer;
var Relations, Connotations: TList;
    C: TConnotation;
    I: integer;
begin
    {First:  find the relations binding the stuff inside the chunk}
    Relations:=GetRelationsThatBindChunk;
    {Second: find the set of Connotations these relations are using.
    Notice that not all connotations are used: for example, 2x5 creates
    a 10 as a TResult, but the 10 should not go back to STM}

    Connotations:=GetConnotationsFromTheseRelations(Relations);
    {Third: Commit these Connotations back to STM; destroy everything in
    the Chunk
    Scan the chunk, deleting everything for which the template does not match
    the desired set of connotations}

    STM_Content.Remove(self);
    STM_Content.Pack;
    for I := 0 to Connotations.Count - 1 do
    begin
        C:=Connotations.Items[i];
        STM_Content.Add(C);
    end;
    {Now delete the chunk baby!}
    ConnotationsAtThisLevel.Clear;
end;

function TChunk.GetConnotationsFromTheseRelations(Relations: TList): TList;
var I, I2:integer;
    R:TRelation;
    C:TConnotation;
    List:TList;
begin
    List:=Tlist.Create;
    For I:=0 to Relations.Count-1 do
    begin
         R:=Relations.Items[I];

         {or simply List:=R.BottomUpItems ???}
         for I2 := 0 to R.OriginalElements.Count - 1 do
         begin
             C:=R.OriginalElements.Items[I2];
             List.Add(C);
         end;
    end;
    result:=list;
end;

function TChunk.GetRelationsThatBindChunk: TList;
var L:TList;
  I: Integer;
  C: TConnotation;
begin
    L:=Tlist.Create;
    for I := 0 to ConnotationsAtThisLevel.Count - 1 do
    begin
        C:=ConnotationsAtThisLevel.Items[i];
        if C.InheritsFrom(TRelation) then
          L.Add(C);
    end;
    Result:=L;
end;

function TChunk.GetConnotationOfType(T: TClass): TConnotation;
Var C:TConnotation; I: integer;
begin
   for I := 0 to ConnotationsAtThisLevel.Count - 1 do
   begin
     C:=ConnotationsAtThisLevel.Items[i];
     if C.ClassType=T then
        result:=C;
   end;
end;

function TChunk.Print:string;
var S:string; I:integer; Relations:TList; C:TConnotation;
begin
    S:='(';
    Relations:=GetRelationsThatBindChunk;
    For I:= 0 to Relations.Count-1 do {For each relation}
    begin
         C:=Relations.Items[i];
         S:=S+C.StringName;
    end;
    result:=S+')';
end;

function TChunk.StringName: string;
begin
    result:=print;
end;

Procedure TChunk.ChunkRelation(R:TRelation);
begin
     AcquireConnotations(R.OriginalElements);
     AcquireConnotations(R.NewElements);
end;

{
Thoughts on the TProposal refactoring...
(i) how to include proposed structures without chunking them up?
        just create the list in a proposal object
(ii) how to accept proposed structures?
        check the proposal's value, and, if accepted, create a chunk?

CAN IT WORK FOR ALL CONNOTATIONS?  ONLY RELATIONS?

    well, I can have 50 copies of random proposals full of lists
    running around, as long as they don't change STM.  As long as
    nothing is taken from STM, and no chunk is created, thousands of
    proposals can be pointing to some cool stuff to do there, irresponsibly

    But sometimes we do create a bond without chunking it up in copycat,
    or maybe in chess (attacks, etc.) So perhaps we could have a commit for
    some relations.

    A new TRelation: How is it attached to anything?  Well, it
    has a List of the original connotations, and a list of the newly created
    connotations.

    A new Chunk: It either groups things based on some (set of) relations. Are
    chunks also relations?  What would the advantages be?  Interface advantages?

    A new Property:  Created when a new relation or a new chunk is created,
    automaticaly bound to a relation or a chunk


We have to do it bottom-up & top-down!

When Bottom-up, we just get things from EM and STM (never inside chunks)

When top-down, we also look inside the chunks, because we might want to
break them apart (and maybe recreate directly the new interpretation? like
a necker cube or a faces/vases illusion)?

1. TRelation should NOT create a chunk: it should be created when
    TCHUNK.conditions are satisfied;
2. A Chunk should find its own properties and relations (and chunks) and Commit
3. TRelation should commit to STM (just like chunks; but without deleting
    anything)
4. Separation of Creation to Commit to STM
5. Shared interface between TRelation and TChunk?
    does a chunk have EVERYTHING that a relation has????
    A chunk has a crucial difference, as it actually deletes stuff from STM

}



{ TRelation }

Constructor TRelation.create;
begin
    Inherited;
    AcceptableConnotations:=Tlist.Create;
    OriginalElements:=TList.Create;
    NewElements:=TList.Create;
end;

procedure TRelation.SearchForInstance;
begin
     GetOriginalElements; {finds related stuff}
     BuildItUp; {generates new connotation & chunks it up}
end;

Function TRelation.ConnotationIsAcceptable(C:TConnotation):boolean;
var ConnotationIsValid:boolean; y:integer;
begin
     ConnotationIsValid:=False;
     GetAcceptableConnotationsTypes;
     for y:=0 to AcceptableConnotations.count-1 do
        if C.Contains(AcceptableConnotations.items[y]) then
            ConnotationIsValid:=true;
     Result:=ConnotationIsValid;
end;

Procedure TRelation.GetOriginalElements;
var C: TConnotation;
begin
    {looks into STM for random connotations... i.e., bricks/results, etc}
    {Currently NOT looking into ExternalMemory, but it should, and perhaps the
    change can be made simply by changing the List from STM_content to EM_Content
    A simple function call could return the appropriate lists on which a connotation
    may be found}
    OriginalElements:=Tlist.Create;
    While (not ConditionsAreSatisfied) do
    begin
         C:=STM_Content.items[Random(STM_Content.Count)];
         if (ConnotationIsAcceptable(C)) and (OriginalElements.IndexOf(C)<0) then
               OriginalElements.Add(C);
    end;
end;

procedure TRelation.BuildItUp;
var RelationItems: Tlist; C: TChunk;
begin
    if ConditionsAreSatisfied then
    begin
         RelationItems:=GetRelatedItems;
         {Remove those guys from BottomUpItems from STM
          ONLY WHEN STATE= Build}
         {(i) compute the relation, then
          (ii) create a new connotation with the correct value and type TResult}
          RelationItems:=ComputeRelation(RelationItems);

         {(iii) Chunk up everything and throw in STM}

         {Why the chunk is necessary, and the relation alone won't make it:
         if you have a relation in STM, then it joins items and generates
         new information; this new information should NOT float around in STM...
         but be tied to the original items and their relation

         then we fall into one problem... how to find out if this specific chunk is
         incompatible with other proposed/built chunks?
         look at the chunk's structure, and for each thing in it, see if there
         are other chunks with the same thing
         --or perhaps include a ChunkedIn:Tchunk inside each connotation}

         C:=TChunk.create;
         C.ChunkRelation(self);

         {Commit to STM, removing from STM the original objects inside C.ConnotationsAtThisLevel}
         C.CommitToSTM;
    end;
end;

function TRelation.MakeRelationChunk(RelationItems:Tlist):TChunk;
var Chunk: TChunk;
begin
     Chunk:=TChunk.Create;
     Chunk.AcquireConnotations(OriginalElements);
     Chunk.AcquireConnotations(NewElements);
     result:=Chunk;
end;


function TRelation.StringName: string;
var I:integer; s:string; C: TConnotation;
begin
    For I:= 0 to OriginalElements.Count-1 do
    begin
        C:=OriginalElements.Items[i];
        S:=S+C.Print;
        if (I<OriginalElements.Count-1) then
           S:=S+self.Print;
    end;
    result:=S;
end;

{ TProperty }

Function TProperty.TopDownSeekFor(C: TConnotation):TConnotation;
begin
     ExpectedConnotation:=C;
     ExpectedConnotationFound:=nil;

     if ExpectedConnotation.InheritsFrom(TProperty) then
     begin
        if (ExactValueCheck(TProperty(ExpectedConnotation))) then {EXACT VALUE CHECKING}
        begin
          {result must point to the right connotation where the thing was found!}
          ExpectedConnotationFound := self;
        end;
     end;
     result:=ExpectedConnotationFound;
end;

{ TSameThing }

function TSameThing.Compare(Chunk1, Chunk2: TChunk): Boolean;
var X:integer;
  I: Integer;
begin
  C1:=Chunk1;
  C2:=Chunk2;
  res:=true;
  x:= C1.AllChunkConnotations.Count;
  if (x=C2.AllChunkConnotations.Count) then {if the counts match, go on...still true...}
  begin
    for I := 0 to C2.AllChunkConnotations.Count - 1 do
    begin
        Connotation1:=C1.AllChunkConnotations.Items[i];
        index:=i;
        ComparisonSubroutine;
    end;
  end else res:=false;
  result:=res;
end;

{ TSet }

procedure TSet.ComparisonSubroutine;
begin
     if (not C2.Contains(Connotation1.ClassType)) then
         res:=false;
end;

{ TTemplates }

procedure TTemplates.ComparisonSubroutine;
begin
    Connotation2:=C2.AllChunkConnotations.Items[index];
    if (Connotation1.ClassType<>Connotation2.ClassType) then
        res:=false;
end;

{ TChunks }

procedure TChunks.ComparisonSubroutine;
begin
    Connotation2:=C2.AllChunkConnotations.Items[index];
    if (Connotation1.ClassType=Connotation2.ClassType) then
    begin
        if (Connotation1.InheritsFrom(TProperty)) then
        begin
          if (not TProperty(Connotation1).ExactValueCheck(TProperty(Connotation2))) then
              res:=false;
        end;
    end else res:=false;
end;                                   

end.
