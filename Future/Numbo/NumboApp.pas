unit NumboApp;

interface

uses CoderackUnit, RelativeProbabilityUnit, Activation1Unit, node1unit,  NumboSlipnet1Unit,  Chunk1Unit, WorkingMemory1Unit,  oldtrees1unit,  HedonicUnit,

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Spin{, math};

  const size_display_nodes = 100;

type
  TForm1 = class(TForm)
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Memo2: TMemo;
    Button3: TButton;
    Button4: TButton;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Shape15: TShape;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button5: TButton;
    Button6: TButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    Image1: TImage;
    Button7: TButton;
    Button8: TButton;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    TabSheet4: TTabSheet;
    Memo3: TMemo;
    Button9: TButton;
    SpinEdit7: TSpinEdit;
    Image2: TImage;
    Memo4: TMemo;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    TabSheet5: TTabSheet;
    Memo5: TMemo;
    Button10: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Image3: TImage;
    TabSheet6: TTabSheet;
    Memo6: TMemo;
    Button23: TButton;
    Button24: TButton;
    SpinEdit8: TSpinEdit;
    Button25: TButton;
    Button26: TButton;
    Procedure ActDraw;
    Procedure DrawSizes;
    Procedure Spread_Activation;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    Procedure DrawHistogram;
    procedure Image1Click(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TestActivation(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure TestChunk(Sender: TObject);
    procedure Command_GET_A(Sender: TObject);
    procedure Command_GET_B(Sender: TObject);
    procedure Command_compare(Sender: TObject);
    procedure Switch(Sender: TObject);
    procedure yes(Sender: TObject);
    procedure Nothing1(Sender: TObject);
    procedure Nothing2(Sender: TObject);
    procedure Init(Sender: TObject);
    procedure No(Sender: TObject);
    procedure OptimumCommand(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    Procedure display_Promising (x, y:integer; P:real);
    procedure Button26Click(Sender: TObject);



  private
    { Private declarations }
  public
    { Public declarations }
    N1, N2, N3, N4, N5: Tnode;
    L1, L2, L3, L4, L5: TLink;
    Slipnet: TNumboslipnet;
    decayable: boolean;
    list1, list2, list3, list4, list5, list6, list7: TChunk;
    Num1, num2, num3, num4, num5, num6, num7: TChunk;

   {delete later}
   Work_Hedonic:WM_Hedonic;
   Command_2_getval_A: THedonic_Command_Get_Value_A;
   Command_2_getval_B: THedonic_Command_Get_Value_B;
   Command_2_compare: THedonic_Command_Compare;
   Command_2_Switch: THedonic_Command_Switch;
   Command_2_Yes: THedonic_Command_Yes;
   Command_2_No:THedonic_Command_No;
   Command_2_Nothing1:THedonic_Command_Nothing1;
   Command_2_Nothing2:THedonic_Command_Nothing2;
   Command_Optimum: THedonic_MacroCommand;

   Coderack:Tcoderack;

   Rel_Prob_tester: TRelative_Probability;

  end;

  function textmake(x: integer):string;
  Function Get_Random_Operation:TopType; {REFACTOR TO CLASS, BABY!}

var
  Form1: TForm1;

implementation
{$R *.dfm}

Procedure TForm1.DrawSizes;
var x:integer;
begin
     x:=round(N1.activation.Get_Level*size_display_nodes);
     Shape11.height:=x; Shape11.Width:=x;
     x:=round(N2.activation.Get_Level*size_display_nodes);
     Shape12.height:=x; Shape12.Width:=x;
     x:=round(N3.activation.Get_Level*size_display_nodes);
     Shape13.height:=x; Shape13.Width:=x;
     x:=round(n4.activation.Get_Level*size_display_nodes);
     Shape14.height:=x; Shape14.Width:=x;
     x:=round(N5.activation.Get_Level*size_display_nodes);
     Shape15.height:=x; Shape15.Width:=x;
     form1.Refresh;
end;


Procedure TForm1.Spread_Activation;
begin
     N1.Notify(N1.activation);
     N2.Notify(N2.activation);
     N3.Notify(N3.activation);
     N4.Notify(N4.activation);
     N5.Notify(N5.activation);

     N1.activation.DeployChange;
     N2.activation.DeployChange;
     N3.activation.DeployChange;
     N4.activation.DeployChange;
     N5.activation.DeployChange;
end;

Procedure TForm1.ActDraw;
begin
     Spread_activation;
     while (N1.activation.Get_Level+n2.activation.Get_Level+n3.activation.Get_Level+n4.activation.Get_Level+n5.activation.Get_Level>0) do
     begin
          Spread_activation;

          N1.activation.decay;
          N2.activation.decay;
          N3.activation.decay;
          N4.activation.decay;
          N5.activation.decay;

          Drawsizes;
     end;
end;

procedure TForm1.Button3Click(Sender: TObject);var y: real;
s1: string;

begin
     Button4.Enabled:=true;
     N1:= Tnode.Create;
     N2:= Tnode.Create;
     N3:= Tnode.Create;
     N4:= Tnode.create;
     N5:= Tnode.create;

     L1:=Tlink.create(N2, 1);
     L2:=Tlink.create(N3, 0.9);
     L3:=Tlink.create(N4, 0.9);
     L4:=Tlink.create(N5, 1);
     L5:=Tlink.create(N1, 0.9);

     N1.RegisterObserver(L1);
     N2.RegisterObserver(L2);
     N3.RegisterObserver(L3);
     N4.RegisterObserver(L4);
     N5.RegisterObserver(L5);


     N1.activation.increase(1);
     Spread_Activation;

     y:= N1.activation.Get_Level;
     str(y:2:2, s1);
     s1:= 'Activation level of N1 is '+s1;
     memo2.Lines.Add(s1);

     y:= N2.activation.Get_Level;
     str(y:2:2, s1);
     s1:= 'Activation level of N2 is '+s1;
     memo2.Lines.Add(s1) ;

     y:= N3.activation.Get_Level;
     str(y:2:2, s1);
     s1:= 'Activation level of N3 is '+s1;
     memo2.Lines.Add(s1);

     y:= N4.activation.Get_Level;
     str(y:2:2, s1);
     s1:= 'Activation level of N4 is '+s1;
     memo2.Lines.Add(s1);

     y:= N5.activation.Get_Level;
     str(y:2:2, s1);
     s1:= 'Activation level of N5 is '+s1;
     memo2.Lines.Add(s1);

     ActDraw;

     memo2.Lines.Add('AND now...');
     y:= N1.activation.Get_Level;
     str(y:2:2, s1);
     s1:= 'Activation level of N1 is '+s1;
     memo2.Lines.Add(s1);

     y:= N2.activation.Get_Level;
     str(y:2:2, s1);
     s1:= 'Activation level of N2 is '+s1;
     memo2.Lines.Add(s1) ;

     y:= N3.activation.Get_Level;
     str(y:2:2, s1);
     s1:= 'Activation level of N3 is '+s1;
     memo2.Lines.Add(s1);

     y:= N4.activation.Get_Level;
     str(y:2:2, s1);
     s1:= 'Activation level of N4 is '+s1;
     memo2.Lines.Add(s1);

     y:= N5.activation.Get_Level;
     str(y:2:2, s1);
     s1:= 'Activation level of N5 is '+s1;
     memo2.Lines.Add(s1);

end;

procedure TForm1.Button4Click(Sender: TObject);
begin
     N1.activation.increase(1);
     N4.activation.increase(0.50);
     Actdraw;
end;



procedure TForm1.Button1Click(Sender: TObject);
begin
     Button1.Enabled:=false;
     Button2.Enabled:=true;
     Button6.Enabled:=true;
     Slipnet:=TNumboSlipnet.create;

     Memo1.Lines.Add('creating slipnet nodes 0...150');
     Slipnet.Create_Number_Nodes;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
     Button2.Enabled:=false;
     Button5.Enabled:=true;

     Memo1.Lines.Add('creating proximity links');
     Slipnet.create_proximity_links;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
     Button5.Enabled:=false;
     Button8.Enabled:=true;
     Slipnet.Create_SalientNode_Links;
end;



procedure TForm1.Button8Click(Sender: TObject);
begin
     Button8.Enabled:=false;
{     Button7.Enabled:=true;}
     Slipnet.Create_Multiplication_Nodes_and_Links;
end;


procedure TForm1.Button6Click(Sender: TObject);
var x:integer; s1, s2:string; Node, NAux: Tnode; Link_Aux: TLink;
begin
     {Achar os 6 nós numéricos e ativá-los 100%}
     Node:=Slipnet.GETNode(spinedit1.Value);
     Node.activation.increase(1);
     Node:=Slipnet.GETNode(spinedit2.Value);
     Node.activation.increase(1);
     Node:=Slipnet.GETNode(spinedit3.Value);
     Node.activation.increase(1);
     Node:=Slipnet.GETNode(spinedit4.Value);
     Node.activation.increase(1);
     Node:=Slipnet.GETNode(spinedit5.Value);
     Node.activation.increase(1);
     Node:=Slipnet.GETNode(spinedit6.Value);
     Node.activation.increase(1);

     {Propagar ativação}
     Slipnet.Spread_Activation;

     {Mostrar tabela no memo, com No e valor de ativação}
     for x:= 0 to 150 do
     begin
          str (x,s1);
          Node:=Slipnet.GETNode(s1);
          str(Node.activation.Get_Level:2:6,s2);
          memo1.Lines.Add(s2+',   '+s1)
     end;

     for x:= 0 to 6 do
     begin
          str (x*25,s1);
          Image1.Canvas.TextOut(1+(150*x),120,s1);
     end;

     x:= 66;
     Node:= Slipnet.GETNode(x);
     str (Node.Number_Of_Connotations, s1);
     memo1.Lines.Add('Number of connotations of '+Node.Name+':'+S1);

     for x:= 0 to node.Number_of_Connotations-1 do
     begin
          Link_aux:=Node.Associations.items[x];
          Naux:=Link_aux.Destination;
          str(Naux.activation.Get_Level:2:4, s1);
          memo1.Lines.Add(Naux.name+' Activated to: '+s1);
     end;

     Drawhistogram;
     decayable:=true;
end;

Procedure Tform1.DrawHistogram;
var x, x1, x2, y1, y2: integer; sum: real; s1:string; Node:Tnode;
begin
     for x:= 0 to 6 do
     begin
          str (x*25,s1);
          Image1.Canvas.TextOut(1+(150*x),120,s1);
     end;

     sum:=0;
     for x:=0 to 150 do
     begin
          x1:=6+(6*x); x2:=x1+6;
          y1:=110;

          str(x,s1);
          Node:=Slipnet.GETNode(S1);
          sum:=sum+Node.activation.get_level;
          y2:=round(110-100*Node.activation.Get_Level);
          image1.Canvas.Rectangle(x1,y1,x2,y2);
          decayable:=(sum>0.001);
     end;
end;


procedure TForm1.Image1Click(Sender: TObject);
begin
     while (decayable) do
     begin
          Slipnet.Decay;
          DrawHistogram;
          Form1.Repaint;
     end;
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
     Slipnet.Decay;
     DrawHistogram;
     Image1.Repaint;
     Form1.Repaint;
end;

procedure TForm1.TestActivation(Sender: TObject);
var x,x1,y1: integer; s1, s2: string; A: Tactivation;
begin
     A:= TActivation.Create;
     for x:= 1 to spinedit7.Value do
     begin
          A.increase(1/spinedit7.Value);
          A.DeployChange;
          str(x/spinedit7.Value:2:2, s1);
          str(A.Get_Level:2:4, s2);
          memo3.Lines.Add('For a current_state of '+s1+', activation is '+s2);
          x1:= round ((x/spinedit7.Value) * (400));
          y1:= round ((A.Get_Level) * (400));
          Image2.Canvas.MoveTo(x1,400);
          Image2.Canvas.LineTo(x1,400-y1);
     end;
end;

function textmake(x: integer):string;
var s:string;
begin
     str (x,s);
     result:=s;
end;

Function Get_Random_Operation:TopType;
var x:integer;
begin
     x:=random (3);
     result:=sum;
     case x of
          0: result:= sum;
          1: result:= multiply;
          2: result:= subtract;
     end; 
end;

procedure TForm1.TestChunk(Sender: TObject);
var s1:string; x, x1:integer; chunk1:Tchunk; Chunks: Array [1..50] of Tchunk;
begin

     for x:= 1 to 20 do
     begin
          Chunks[x]:=TChunkBrick.Create(random(10),3.3);
     end;

     for x:= 21 to 30 do
     begin
          Chunks[x]:=TChunkList.Create(3.3, Get_Random_Operation); {REFACTOR Get_Random_Operation TO ITS CLASS}
          Chunks[x].Add(Chunks[random(10)+1], 0);
          Chunks[x].Add(Chunks[random(10)+1], 0);
          Chunks[x].Add(Chunks[random(10)+1], 0);
          str (Chunks[x].value, s1); memo4.Lines.Add('value is '+s1);
          memo4.Lines.Add(Chunks[x].Name);
     end;

     x1:=21;
     for x:= 31 to 36 do
     begin
          chunks[x]:=TchunkList.Create(3.0,Get_Random_Operation);
          Chunks[x].Add(Chunks[x1],0);
          Chunks[x].Add(Chunks[x1+1],0);
          x1:=x1+2;
          str (Chunks[x].value, s1); memo4.Lines.Add('value is '+s1);
          memo4.Lines.Add(Chunks[x].Name);
     end;

     chunks[36].Add(chunks[28],0);
     str (Chunks[36].value, s1); memo4.Lines.Add('value is '+s1);
     memo4.Lines.Add(Chunks[36].Name);

     list7:= chunks[36];
     {this works for "planar" trees; why will it not work for deep ones?}
     for x:= 0 to list7.Size_of_tree do
     begin
         memo4.Lines.Add('--');
         x1:=list7.Get_random_Position;
         memo4.Lines.Add(textmake(x1)+' out of '+textmake(list7.Size_of_tree));
         Chunk1:=tchunk(list7.Get_Item_in_Position(x1));
         memo4.Lines.Add(chunk1.name);
         {list7.Remove(chunk1);}
         memo4.Lines.Add(list7.name);


{         x1:=list7.Get_Random_Insert_Position;
         list7.InsertChunk_in_Position(Chunk1,x1);
         memo4.Lines.Add(list7.name);}
     end;

end;




procedure TForm1.Button11Click(Sender: TObject);
var s1:string; x, x1:integer;
    WorkingMemory: TWorkingMemory;
    WM_aux: Tchunk;
    nums: array[1..5] of tchunk;
    Chunk, chunk1: TChunk;

begin
     {NOT WORKING!!!}
     {Load bricks into working memory}
     nums[1]:=TChunkBrick.create(spinedit1.Value, 1.0);
     nums[2]:=TChunkBrick.create(spinedit2.Value, 1.0);
     nums[3]:=TChunkBrick.create(spinedit3.Value, 1.0);
     nums[4]:=TChunkBrick.create(spinedit4.Value, 1.0);
     nums[5]:=TChunkBrick.create(spinedit5.Value, 1.0);

     WM_Aux:= TChunkList.create(1.0, OpNone);
     WorkingMemory:= TWorkingMemory.create(WM_aux);

     for x:= 1 to 5 do
         WorkingMemory.WM.ChunkStuff.Add(Nums[x]);

     memo4.Lines.Add('Working memory: bricks');
     for x:=0 to workingmemory.WM.ChunkStuff.count-1 do
     begin
          Chunk:= Workingmemory.wm.chunkstuff.items[x];
          memo4.Lines.Add(Chunk.name);
     end;

     {Play with chunks, creating & destroying...}

     memo4.Lines.Add('Chunks coming up...');

     for x:= 1 to 10 do begin
{            workingmemory.Random_ChunkList_Includer;}
         {workingmemory.Random_Chunk_Mover;}

         x1:=workingmemory.Wm.Get_random_Position;
         Chunk1:=Tchunk(workingmemory.WM.Get_Item_in_Position(x1));
{         memo4.Lines.Add('Deleting '+Chunk1.name);}
        (* memo4.Lines.Add({'Before: '+}Workingmemory.WM.name);*)
         workingmemory.WM.Remove(Chunk1);
{         memo4.Lines.Add('After: '+Workingmemory.WM.name);}
         x1:=workingmemory.WM.Get_Random_Insert_Position;
         workingmemory.Wm.InsertChunk_in_Position(Chunk1,x1);
{         memo4.Lines.Add(Chunk1.name);}
         memo4.Lines.Add(Workingmemory.WM.name);
     end;

     str(Workingmemory.wm.size_of_tree, S1);
     memo4.Lines.Add('Number of bricks & chunks:'+s1);
end;


procedure TForm1.Command_Get_A(Sender: TObject);
begin
     {Implements command get A}

     Command_2_Getval_A.execute;
end;

procedure TForm1.Command_GET_B(Sender: TObject);
begin
     {Implements command get B}

     Command_2_Getval_B.execute;
end;

procedure TForm1.Command_compare(Sender: TObject);
begin
      {a<B?}

     Command_2_Compare.execute;
end;


procedure TForm1.Switch(Sender: TObject);
begin
{Switches A and B}

     Command_2_Switch.execute;
end;

procedure TForm1.yes(Sender: TObject);
begin
{yes}

     Command_2_Yes.execute;
end;

procedure TForm1.Nothing1(Sender: TObject);
begin
     {Nothing 1}

     Command_2_Nothing1.execute;
end;

procedure TForm1.Nothing2(Sender: TObject);
begin
{Nothing2}

     Command_2_Nothing2.execute;
end;


procedure TForm1.No(Sender: TObject);
begin
     {no command}

     Command_2_No.execute;
end;


procedure TForm1.Init(Sender: TObject);
begin
{Init}
     Work_Hedonic:=WM_Hedonic.create;
     Randomize;

     Button10.Enabled:=true;
     Button14.Enabled:=true;
     Button15.Enabled:=true;
     Button16.Enabled:=true;
     Button17.Enabled:=true;
     Button18.Enabled:=true;
     Button19.Enabled:=true;
     Button20.Enabled:=true;
     Button22.Enabled:=true;

     Button25.Enabled:=true;


     Command_2_Getval_A:=THedonic_Command_Get_Value_A.create;
     Command_2_Getval_A.send_Memo(Memo5);
     Command_2_Getval_A.Get_Value_in_A(Work_Hedonic);

     Command_2_Getval_B:=THedonic_Command_Get_Value_B.create;
     Command_2_Getval_B.send_Memo(Memo5);
     Command_2_Getval_B.Get_Value_in_B(Work_Hedonic);

     Command_2_Compare:=THedonic_Command_Compare.create;
     Command_2_Compare.send_Memo(Memo5);
     Command_2_Compare.Compare(Work_Hedonic);

     Command_2_Switch:=THedonic_Command_Switch.create;
     Command_2_Switch.send_Memo(Memo5);
     Command_2_Switch.Switch(Work_Hedonic);

     Command_2_Yes:=THedonic_Command_Yes.create;
     Command_2_Yes.send_Memo(Memo5);
     Command_2_Yes.Exec_Yes(Work_Hedonic);

     Command_2_No:=THedonic_Command_No.create;
     Command_2_No.send_Memo(Memo5);
     Command_2_No.Exec_No(Work_Hedonic);

     Command_2_Nothing1:=THedonic_Command_Nothing1.create;
     Command_2_Nothing1.send_Memo(Memo5);
     Command_2_Nothing1.Do_Nothing1(Work_Hedonic);

     Command_2_Nothing2:=THedonic_Command_Nothing2.create;
     Command_2_Nothing2.send_Memo(Memo5);
     Command_2_Nothing2.Do_Nothing2(Work_Hedonic);


     Command_Optimum:=THedonic_MacroCommand.create;
     Command_Optimum.send_Memo(Memo5);
     Command_Optimum.LoadNewCommand(Command_2_Getval_A);
     Command_Optimum.LoadNewCommand(Command_2_Getval_B);
     Command_Optimum.LoadNewCommand(Command_2_Compare);
     Command_Optimum.LoadNewCommand(Command_2_Yes);
     Command_Optimum.LoadNewCommand(Command_2_Switch);


     Repertoire:=THedonic_Macrocommand.create;
     Repertoire.send_Memo(Memo5);
     Repertoire.LoadNewCommand(Command_2_Getval_A);
     Repertoire.LoadNewCommand(Command_2_Getval_B);
     Repertoire.LoadNewCommand(Command_2_Compare);
     Repertoire.LoadNewCommand(Command_2_Yes);
     Repertoire.LoadNewCommand(Command_2_Switch);

     Coderack:=TCoderack.Create;

end;

procedure TForm1.OptimumCommand(Sender: TObject);
begin
     Command_Optimum.execute;
end;

procedure TForm1.Button23Click(Sender: TObject);
var feedback:real; S:String;
begin
     Rel_Prob_Tester:= TRelative_Probability.create;
     form1.Button24.Enabled:=true;
     feedback:=spinedit8.value/100;
     Rel_Prob_Tester.feedback(feedback);
     feedback:=Rel_Prob_Tester.get_Fitness;
     str (Feedback:2:2, S);
     Memo6.Lines.Add(S);
end;

procedure TForm1.Button24Click(Sender: TObject);
var t:integer; feedback: real; S:String;
begin
     for t:= 1 to 10 do
     begin
          feedback:=Rel_Prob_Tester.get_Fitness;
          str (Feedback:2:2, S);
          Memo6.Lines.Add(S);
     end;

end;

Procedure TForm1.display_Promising (x, y:integer; P:real);
var x1, x2, y1, y2, diff: integer;
begin
     x1:=20+20*x;
     y1:=20+20*y;
     diff:= round (10*p);
     if P<0 then Image3.Canvas.Pen.Color:= clred else Image3.Canvas.Pen.Color:= clgreen;
     Image3.Canvas.Ellipse(x1-diff,y1-diff,x1+diff,y1+diff);
     Image3.Canvas.Pen.Color:= clgray;

     diff:=10;
     Image3.Canvas.moveto (0,100+diff);
     Image3.Canvas.lineto (100+diff, 100+diff);
     Image3.Canvas.lineto (100+diff, 0);
end;

procedure TForm1.Button25Click(Sender: TObject);
var x, y, x_Index, y_index:integer; Cx, Cy: Icommand; P:TRelative_Probability;
begin

     Coderack.Get_New_Command;

     Coderack.ExecuteCommand;

     Image3.Canvas.Rectangle(0,0,1000,1000);

     for x:= 0 to repertoire.Commands.Count-1 do
     begin
          Cx:= repertoire.Commands.items[x];
          x_index:=x;
          for y:= 0 to Cx.Promising_List.Count-1 do
          begin
               Cy:= Cx.Promising_List.items[y];
               P:=Cx.Promising_Probability_list.items[y];
               Y_index:=repertoire.Commands.indexof(Cy);
               display_Promising (x_index,y_Index, P.get_Fitness);
          end;
     end;
end;

procedure TForm1.Button26Click(Sender: TObject);
var x: integer; Cx: Icommand;
begin
     memo6.Lines.Clear;
     for x:= 0 to repertoire.Commands.Count-1 do
     begin
          Memo6.Lines.Add('------------------');
          Cx:=repertoire.commands.items[x];
          Memo6.Lines.Add(Cx.GetFullCommandName);
     end;
end;

end.











