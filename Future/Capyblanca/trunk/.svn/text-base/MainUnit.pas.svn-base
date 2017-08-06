unit MainUnit;

interface

uses
  AbstractImpulses, ExternalMemoryUnit, Workingmemoryunit, ImpulseSquareAndPiece,BasicImpulse,
  BoardViewerUnit, StdCtrls, Controls, ExtCtrls, ComCtrls, Classes, Windows, Messages, SysUtils,
  Graphics, Forms, Dialogs, types, Grids, SpecialFunctions, Buttons, Spin, SlipnetUnit;

type
  TBoardLoader = class(Tobject)
                             BoardCanvas:TCanvas;
                             constructor create;
                       end;

                                                                
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    BoardViewer: TCheckBox;
    Objects: TCheckBox;
    Concepts: TCheckBox;
    ParallelPriorities: TCheckBox;
    Temperature: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    TabSheet3: TTabSheet;
    Button3: TButton;
    Button4: TButton;
    StringGrid1: TStringGrid;
    Memo1: TMemo;
    Play: TBitBtn;
    Mobility: TLabel;
    ProgressBar1: TProgressBar;
    NumberofImpulses: TLabel;
    UrgesRemaining: TLabel;
    PaintBox1: TPaintBox;
    Button5: TButton;
    OpenDialog1: TOpenDialog;
    UpDown1: TUpDown;
    Edit1: TEdit;
    UpDown2: TUpDown;
    Edit2: TEdit;
    UpDown3: TUpDown;

    Procedure ImpulsiveUrges;
    procedure ImpulsiveThoughtsInRandomOrder;
    procedure ImpulsiveThoughtsByUrgency;
    procedure SomeImpulsiveThoughtsByUrgency(Numberimpulses:integer);
    procedure BoardViewerClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormPaint(Sender:TObject);
    procedure StartCognition;
    procedure Button3Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PlayClick(Sender: TObject);
    procedure BoardProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; const R: TRect;
      const Msg: String);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);        
    procedure PaintBox1Click(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure test_CanSomeoneHelpMeHerePlease;

var
  Form1: TForm1;

  numImpulses:integer;
  Temperature: TTemperature;

  boardloader:Tboardloader;

implementation
{$R *.DFM}


procedure TForm1.FormCreate(Sender: TObject);
begin
     Urges:=Tlist.create; {Creates the urges -- list of parallel priorities}
     ExternalMemory:=TexternalMemory.create;
     Workingmemory :=Tworkingmemory.create;

     randseed:=-521942864; {good output, but too rapid discovery of promotion}
     randseed:=-1839407778;{good output, but bishop does not get minimized ever}
     randseed:=250374040; {nice, but bad final output, because of bishop's prominence}
     randseed:=-899645438; {what's interesting here? output is great, but most important relations are found first...}

     randseed:=-2135133625; {test again?}
     randseed:=953926607;
     //randomize;



     Play.enabled:=false;
     Workingmemory.Alreadythinking:=false;

end;

procedure Tform1.FormPaint(Sender:TObject);
var t,x,y,x1,x2,code, PieceRolescount:integer; s, s1:string; p:tsquare; sq:^tsquare;
    TABS:TParamabstractRole; Piece1:tpiece; {NewCanvas:^Tcanvas; Rect:Trect;}
begin
{      new (newcanvas);
      NewCanvas^:=Tcanvas.Create;
      Rect.Left:=0; Rect.Right:=400; Rect.Top:=0; Rect.Bottom:=400;
      Newcanvas^.FrameRect(Rect);}

      paintbox1.Canvas.brush.Color:=clblue;
      Paintbox1.Canvas.Rectangle(0,0,400,400);

      p.x:=1;
      p.y:=1;
      if (workingmemory.pieces.count>0) then
      begin
      {str (}
      val(edit2.text,x,code);
      Piece1:= workingmemory.pieces.items[x-1];
      for x:= 1 to 8 do
         for y:= 1 to 8 do
         begin
              str(Piece1.Mobility[x,y].distance,s);
              stringGrid1.Cells[x-1,y-1]:=S;
         end;
      Form1.Mobility.Caption:='Mobility of '+Piece1.FullId;
      end;


      for x:=1 to 8 do
          for y:= 1 to 8 do
          begin
               if workingmemory.saccaded[x,y] then
               begin
                    Drawsquare(Paintbox1.Canvas,x-1,y-1);
                    DrawSquareName(Paintbox1.Canvas,x-1,y-1);
               end;

               if workingmemory.glancedshadow[x,y] then
               begin
                    Drawshadow(Paintbox1.Canvas,x-1,y-1);
                    workingmemory.glancedshadow[x,y]:=false;
               end;

          end;

      for t:= 0 to WorkingMemory.Pieces.count-1 do
      begin
           Piece1:= workingmemory.pieces.items[t];
           Drawsquare(Paintbox1.Canvas,Piece1.sq.x-1,Piece1.sq.y-1);
           Piece1.display(Paintbox1.Canvas);
           DrawSquareName(Paintbox1.Canvas,Piece1.sq.x-1,Piece1.sq.y-1);
      end;

      for x1:=0 to workingmemory.pieces.count-1 do
      begin
           piece1:=workingmemory.Pieces.items[x1];
           if (piece1.abstractroles.count>0) then
           begin
                for x2:= 0 to piece1.abstractroles.count-1 do
                begin
                     TABS:= piece1.abstractroles.items[x2];
                     Draw_Trajectory(Tabs,Paintbox1.Canvas);
                end;
           end;
      end;
      {Paintbox1.Canvas.CopyRect(Rect, NewCanvas^, Rect);}

      {sleep (50);}

      Progressbar1.max:=Numimpulses+urges.count;
      Progressbar1.position:=NumImpulses;
      Progressbar1.Refresh;
      Progressbar1.Repaint;

      str (Numimpulses, S);
      NumberofImpulses.caption:='Number of fired impulses: '+S;
      NumberofImpulses.Repaint;

      str (Urges.count, S);
      UrgesRemaining.caption:='Urges remaining: '+S;
      UrgesRemaining.repaint;


     if (urges.count=0) then
     begin
          Str(workingmemory.Pieces.count,S);
          form1.Memo1.lines.add(S+' created Pieces.');

          Str(Numimpulses,S);
          form1.Memo1.lines.add(S+' impulses fired.');

          {Now I want to list ALL the Abstract Roles Created in order to study them}
          {For each piece, print its abstract roles!}
          PieceRolescount:=0;
          for t:= 0 to WorkingMemory.Pieces.count-1 do
          begin
               Piece1:= workingmemory.pieces.items[t];
               if Piece1.Abstractroles.Count > 0 then
               begin
                    {Display piece here;}
                    form1.Memo1.lines.add('Piece '+Piece1.Fullname+' has '+piece1.numroles+' roles:');
                    for x:= 0 to Piece1.AbstractRoles.Count-1 do
                    begin
                         TABS:=Piece1.AbstractRoles.items[x];
                         {s:=TABS.getAbstractrolename; &&& separate this piece of code by god!!!!!}
                         Case TABS.Roletype of
                              guardian: TABS.rolename:='guardian';
                              attacker: TABS.rolename:='attacker';
                              blocker: TABS.rolename:='blocker';
                              interceptor: TABS.rolename:='interceptor';
                         end;
                         S:=TABS.Rolename;
                         sq:=TABS.Trajectory.items[TABS.Trajectory.Count-1];
                         Memo1.Lines.Add(S+' of '+ squarename(sq^));
                         PieceRolescount:=PieceRolescount+1;
                    end;
               end;

          end;
          Str(PieceRolescount,S);
          form1.Memo1.lines.add(S+' created trajectories.');
     end;
     {test_CanSomeoneHelpMeHerePlease;}
     form1.Repaint;
end;


Procedure TForm1.StartCognition;
var impulse: ^timpulse; pos_x,pos_y:integer; potentialsquare: ^tsquare;

Param: ^TParamCheckSquare;
begin
     {this procedure creates the very first impulse to search for pieces on a random
     square and then transfers control to the main loop}

     Workingmemory.Alreadythinking:=true;
     for pos_x:= 1 to 8 do
         for Pos_y:=1 to 8 do
         begin
              new (impulse);
              new(potentialsquare);
              Potentialsquare^.x:=pos_x;
              Potentialsquare^.y:=pos_y;

              new (Param);
              Param^:=TParamCheckSquare.Create;
              Param.move:=potentialsquare^;
              param.pmemo:=form1.Memo1;
              param.impulsepointer:=impulse;

              Impulse^:=TImpulseCheckSquare.create (param^, 15);
         end;
end;

procedure TForm1.BoardViewerClick(Sender: TObject);
begin
     If BoardViewer.checked then
     begin
          Application.CreateForm(TBoardViewerWindow, BoardViewerWindow);
          BoardViewerWindow.Make(self);
     end;
end;

Procedure Tform1.ImpulsiveUrges;
var Impulse:^Timpulse;
    counter:integer;
    paramimpstring: ^TParamImpString;
    paramimpnumber: ^TParamImpNumber;
begin
     for counter:=0 to 10 do
     begin
          New(Impulse);
          new (ParamImpNumber);
          ParamImpNumber^:=TParamImpNumber.Create;
          paramimpNumber.N:=counter/100;
          paramimpNumber.pmemo:=memo1;
          Paramimpnumber.impulsepointer:=impulse;
          impulse^:=timpulsenumber.create (ParamImpNumber^, counter +1);
     end;
     for counter:=11 to 19 do
     begin
          new (impulse);
          new (ParamImpString);
          ParamImpString^:=TParamImpString.Create;
          paramimpstring.S:='gotcha!';
          paramimpstring.pmemo:=memo1;
          paramimpstring.impulsepointer:=impulse;
          impulse^:=timpulsestring.create(ParamImpString^, counter);
     end;
     for counter:=20 to 30 do
     begin
          New(Impulse);
          new (ParamImpNumber);
          ParamImpNumber^:=TParamImpNumber.Create;
          paramimpNumber.N:=counter/100;
          paramimpNumber.pmemo:=memo1;
          Paramimpnumber.impulsepointer:=impulse;
          impulse^:=timpulsenumber.create (ParamImpNumber^, counter);
     end;
     for counter:=31 to 40 do
     begin
          new (impulse);
          new (ParamImpString);
          ParamImpString^:=TParamImpString.Create;
          paramimpstring.S:='iiiiihhhhaaaaaaa!';
          paramimpstring.pmemo:=memo1;
          paramimpstring.impulsepointer:=impulse;
          impulse^:=timpulsestring.create(ParamImpString^, counter+50);
     end;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
     ImpulsiveUrges;
     Button2.Enabled:=true;
     Button3.Enabled:=true;
     Memo1.Lines.add('=============================================');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
     ImpulsiveThoughtsInRandomOrder;
end;

procedure TForm1.ImpulsiveThoughtsInRandomOrder;
var x:integer; s,s1:string; imp:^timpulse;
begin                                                      
     Urges.pack;
     while (urges.count>0) do
     begin
          urges.pack;
          x:=random(urges.count);
          str(x,s);
          str (urges.count,s1);
          memo1.lines.add('random urge numbered '+S+' taken from set of '+s1);
          Imp:=Urges.items[x];
          Urges.remove(imp);
          Urges.pack;
          imp^.fire;
          Dispose(imp);
          urges.pack;
     end;
end;

procedure TForm1.ImpulsiveThoughtsByUrgency;
var counter:integer;  Imp:^Timpulse; U,x,SumUrgency:Turgency;
begin
     Urges.pack;
     NumImpulses:=0;
     while (Urges.count>0) do
     begin
          sumurgency:=0;
          for counter:=0 to Urges.count-1 do
          begin
               imp:=Urges.items[counter];
               sumurgency:=sumurgency+imp^.urgency;
          end;
          x:= random(sumurgency+1);
          counter:=-1;
          u:=0;
          repeat
                counter:=counter+1;
                Imp:=Urges.items[counter];
                u:=u+imp^.urgency;
          until (u>=X);  {this means that impulse number COUNTER is going to fire!}
          Urges.remove(imp);
          Urges.pack;
          imp^.fire;
          Imp^.free;
          Imp:=nil;
          Dispose(imp);
          NumImpulses:=NumImpulses+1;
          Urges.pack;
     end;
end;

procedure TForm1.SomeImpulsiveThoughtsByUrgency(Numberimpulses:integer);
var counter:integer;  Imp:^Timpulse; U,x,SumUrgency:Turgency;
begin
     Urges.pack;
     NumImpulses:=0;
     while (Urges.count>0) and ((numimpulses<Numberimpulses) or (NumberImpulses=0)) do
     begin
          sumurgency:=0;
          for counter:=0 to Urges.count-1 do
          begin
               imp:=Urges.items[counter];
               sumurgency:=sumurgency+imp^.urgency;
          end;
          x:= random(sumurgency+1);
          counter:=-1;
          u:=0;
          repeat
                counter:=counter+1;
                Imp:=Urges.items[counter];
                u:=u+imp^.urgency;
          until (u>=X);  {this means that impulse number COUNTER is going to fire!}
          Urges.remove(imp);
          Urges.pack;
          imp^.fire;
          Imp^.free;
          Imp:=nil;
          FormPaint(self);
          Dispose(imp);
          NumImpulses:=NumImpulses+1;
          Urges.pack;
     end;
     workingmemory.impulses_fired:=workingmemory.impulses_fired+Numimpulses;
end;


constructor Tboardloader.create;
begin
end;


procedure TForm1.RadioButton2Click(Sender: TObject);
begin
     Boardloader:=Tboardloader.create;
     Paintbox1:=Tpaintbox.create(self);
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
     ImpulsiveThoughtsbyUrgency;
end;


procedure test_CanSomeoneHelpMeHerePlease;
var sq:tsquare; piece:tpiece; list:tlist; x:integer;s:string;

begin
     {Testing new CanSomeOneHelpMeHerePlease function}
     List:=tlist.create;
     sq.x:=8; sq.y:=6;
     Piece:= workingmemory.GetPieceAt(Sq);
     if piece=nil then exit;
     form1.Memo1.Lines.add('            HELP the '+piece.FullId);

     Sq.x:=4;
{     List:=workingmemory.CanSomeoneHelpMeHerePlease(Piece, sq);}
     for x:= 0 to List.count-1 do
     begin
          Piece:=List.items[x];
          s:= piece.fullid;
          form1.Memo1.Lines.add('            '+s);
     end;
end;


procedure TForm1.PlayClick(Sender: TObject);
var s:string; x, code:integer;
begin
     if not(workingmemory.alreadythinking) then
     begin
          str(randseed,S);
          form1.memo1.lines.add('Seed='+S);
     end;

     If not (workingmemory.alreadythinking) then StartCognition;

     {transfering control to main loop:}
     val (edit1.Text,x,code);
     SomeImpulsiveThoughtsByUrgency(x);
end;


procedure TForm1.BoardProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; const R: TRect;
  const Msg: String);
begin
     Form1.FormPaint(self);
end;

procedure TForm1.Button5Click(Sender: TObject);
Var P:Array[1..32] of TExternalPiece; Pos:Tsquare; s:string; s1:char;
    x1,y1: integer; PositionFile: textfile;
    white: boolean; piecetype:tpiecetype;
    numpieces:integer;
begin
     Play.enabled:=true;  Button4.enabled:= true;
     if OpenDialog1.Execute then
        s:= OpenDialog1.FileName;

     ExternalMemory:=TexternalMemory.create;

     AssignFile (PositionFile, s);
     FileMode := fmOpenRead;
     Reset(PositionFile);
     s:='';

     numpieces:=1;
     for x1:=1 to 8 do
     begin
          Readln(positionfile,s);
          for y1:= 1 to 8 do
          begin
                Pos.y:=x1; Pos.x:=y1;
                s1:= s[y1*4-3];
                if s1='W' then white:=true else white:=false;
                s1:= s[y1*4-2];
                case s1 of
                    'P': piecetype:= pawn;
                    'B': piecetype:=bishop;
                    'R': piecetype:=Rook;
                    'K': piecetype:=King;
                    'N': piecetype:=knight;
                    'Q': piecetype:=queen;
                end;

                if ((s1<>'-') and (s1<>' ')) then
                begin
                      P[numpieces]:=TExternalpiece.create (Pos,white,piecetype);
                      numpieces:=numpieces+1;
                end;
          end;
     end;

     for x1:=1 to (numpieces-1) do
         P[x1].destroy;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
     edit1.text:='0';
     Playclick(self);
end;

procedure TForm1.PaintBox1Click(Sender: TObject);
begin
     if button4.Enabled then formpaint(self);
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
var x,code: integer;
begin
     val (Edit2.text, x, code);
     if (x-1>=workingmemory.pieces.count) then
        edit2.text:='1';
     form1.FormPaint(self);
end;

end.
