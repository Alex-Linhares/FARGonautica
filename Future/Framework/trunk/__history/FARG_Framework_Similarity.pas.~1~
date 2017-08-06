unit FARG_Framework_Similarity;



interface

uses FARG_Framework_Chunk, NumboConnotations, classes;

type

  TSimilarity = class
                  private
                  C1, C2: TChunk;              
                  res: boolean;
                  Connotation1, Connotation2: TConnotation;
                  index:integer;

                  public
                  ConnotationSet: TList;
                  Outliers: TList;
                  Delta, Sum: real;
                  OutlierX, OutlierY, MostOutlierX, MostOutlierY:integer;

                  constructor Create;

                  procedure MeasureSetSimilarities;
                  function SumDistances:real;

                  Function GetDelta(X:TNumber; Y: TNumber):real;  overload;  virtual;
                  {Function GetDelta(XCon:TConnotation; YCon: TConnotation):real;  overload; virtual;}


                  function Compare(Chunk1, Chunk2:TChunk):Boolean;
                  procedure ComparisonSubroutine; virtual; abstract;
  end;

  TChunks = class (TSimilarity)
                  procedure ComparisonSubroutine; override;
  end;

  TTemplates = class (TSimilarity)
                  procedure ComparisonSubroutine; override;
  end;

  TSet = class (TSimilarity)
                  procedure ComparisonSubroutine; override;
  end;

implementation

{ TSimilarity }

Constructor TSimilarity.Create;
begin
  ConnotationSet:=TList.Create;
end;



{function TSimilarity.GetDelta(XCon:TConnotation; YCon: TConnotation): real;
begin
  if XCOn.classtype= TConnotation then
    XCon:=TNumber(XCon);
  if YCon.ClassType=TNumber then
    YCon:=TNumber(YCon);
  result:=GetDelta(XCon,YCOn);
end; }






function TSimilarity.GetDelta(X:TNumber; Y: TNumber): real;
var num:real;
begin
    num:=sqrt((x.GetValue+y.GetValue)*(x.getvalue+y.GetValue));
    result:=num;
end;

procedure TSimilarity.MeasureSetSimilarities;
var best, measure: real;
    x, y, bestX, bestY:integer;
    Nx, Ny:TNumber;
begin
  OutlierX:=-1;
  OutlierY:=-1;
  Best:=SumDistances*sumdistances;
  measure:=best;
  for x := 0 to ConnotationSet.Count - 1 do
  begin
    for y := 0 to ConnotationSet.Count - 1 do
    begin
      if x<>y then
      begin
        OutlierX:=x;
        OutlierY:=Y;
        Sum:=SumDistances;

        NX:=ConnotationSet.items[outlierx];
        NY:=ConnotationSet.items[outliery];
        sum:=  (sum+GetDelta(Nx,Ny)) / (sqrt( (sum-GetDelta(NX,NY)) * (sum-GetDelta(NX,NY)) ));

        {sum:= sqrt( (measure-sum)*(measure-sum) );}

        {sum:=SumDistances;}
        {it must be one group versus the other}
        if (Sum<Best) then
        begin
          Best:=sum;
          MostOutlierX:=OutlierX;
          MostOutlierY:=OutlierY;
        end;
      end;
    end;
  end;
end;

function TSimilarity.SumDistances: real;
Var sum: real;
    x,y: integer;
    ConnotationX, ConnotationY: TConnotation;
    Nx, Ny: TNumber;
begin
   sum:=0;
   for x := 0 to ConnotationSet.Count - 1 do
     for y := 0 to ConnotationSet.Count - 1 do
       if (x<>y) and (x<>outlierX) and (y<>outlierY) then
       begin
          ConnotationX:=ConnotationSet.Items[x];
          ConnotationY:=ConnotationSet.Items[y];
          if ConnotationX.ClassType=TNumber then
          begin
            NX:=TNumber(ConnotationX);
            NY:=TNumber(ConnotationY);
            sum:=sum+GetDelta(NX,NY);
          end;
       end;
    result:=sum;
end;

function TSimilarity.Compare(Chunk1, Chunk2: TChunk): Boolean;
var X:integer;
  I: Integer;
begin
  C1:=Chunk1;
  C2:=Chunk2;
  res:=true;
  x:= C1.ElementsAtAllChunkLevels.Count;
  if (x=C2.ElementsAtAllChunkLevels.Count) then {if the counts match, go on...still true...}
  begin
    for I := 0 to C2.ElementsAtAllChunkLevels.Count - 1 do
    begin
        Connotation1:=C1.ElementsAtAllChunkLevels.Items[i];
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
    Connotation2:=C2.ElementsAtAllChunkLevels.Items[index];
    if (Connotation1.ClassType<>Connotation2.ClassType) then
        res:=false;
end;

{ TChunks }

procedure TChunks.ComparisonSubroutine;
begin
    Connotation2:=C2.ElementsAtAllChunkLevels.Items[index];
    if (Connotation1.ClassType=Connotation2.ClassType) then
    begin
        if (Connotation1.InheritsFrom(TValue)) then
        begin
          if (not TValue(Connotation1).ExactValueCheck(TValue(Connotation2))) then
              res:=false;
        end;
    end else res:=false;
end;


end.
