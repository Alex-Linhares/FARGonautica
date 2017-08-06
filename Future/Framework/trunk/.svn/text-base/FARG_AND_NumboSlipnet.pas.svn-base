unit FARG_AND_NumboSlipnet;

interface


uses     FARG_Framework_NodesAsObservers, FARG_FrameworkActivationStrategy,
         FARG_Framework_Chunk, NumboConnotations, Classes, sysutils;

const drag_Multiplication_Node = 0.1;   high_drag_Multiplication_Node=0.1;     Salient_Multiples_10_drag=0.1;

type

         TMultiplication_Node = class (Tnode)
                                      Constructor create_links(N1, N2, NResult: Tnode);
                                end;

         TSlipnet = class
                    protected
                         nodes: tlist;
                         links: tlist;

                    public
                         constructor create;
                         Procedure Spread_Activation;
                         Procedure Create_Link_Between_Nodes (NOrigin, NDest: Tnode; drag: real);
                         function GetNode(C:Tconnotation):TNode;
                         procedure decay;
                    end;


         TNumboSlipnet = class (TSlipnet)
                              procedure Create_Number_Nodes;
                              procedure create_proximity_links;
                              procedure Create_Sum_Nodes_and_Links;
                              Procedure Create_Multiplication_Nodes_and_Links;
                              Procedure Create_SalientNode_Links;
                              function GetNode(x:integer):TNode; overload;
                              Procedure Create_Link_Between_Numbers (num1, num2: integer; drag: real);
                              Procedure Create_Multiplication_Node (x,y: integer);
                              Procedure Create_Multiplication_Decompositions (x: integer);
                         end;

implementation

Constructor Tslipnet.create;
begin
     Nodes:=tlist.Create;
     Links:=tlist.Create;
end;

Procedure TSlipnet.decay;
var x:integer; Node: Tnode;
begin
     for x:= 0 to nodes.count-1 do
     begin
          Node:= nodes.items[x];
          Node.activation.decay;
     end;
end;

Procedure TSlipnet.Spread_Activation;
var x:integer; N: Tnode;
begin
     {Let's spread activation from the top to bottom of the node list first...}
     for x:= nodes.Count-1 downto 0 do
     begin
          N:= Nodes.items[x];
          N.Notify(N.activation);
     end;

     {Now, nodes that just received some activation can propagate it, right, so we
      now propagate the other way around}
     for x:= 0 to nodes.Count-1 do
     begin
          N:= Nodes.items[x];
          N.Notify(N.activation);
     end;

     {Now we need to reset the activations, deploying the changes (I really need to blog about this one!)}
     for x:=0 to nodes.Count-1 do
     begin
          N:= Nodes.items[x];
          N.activation.DeployChange;
     end;
end;

procedure TNumboSlipnet.Create_Number_Nodes;
{Deeply screwed up slipnet--Get the same slipnet from Daniel Defays in here}
var x: integer;  N: Tnode;
begin
     for x:= 0 to 150 do
     begin
          N:=Tnode.create;
          Nodes.Add(N);
     end;
end;

function TSlipnet.GetNode(C:TConnotation):Tnode;
var Connotation: TConnotation; Node: TNode;
  I: Integer;
begin
    i:=0;
    While (I<Nodes.Count) do
    begin
      Node:= Nodes.items[i];
      {
      ERROR IN CODE!!!  Refactor?
      if C.equals(Node.Connotation) then}
          i:=nodes.Count+1;
    end;
    result:=Node;
end;

function TNumboSlipnet.GetNode(X:integer):Tnode;
var Connotation: TNumber; Node: TNode;
  I: Integer;
begin
    Connotation:=TNumber.Create;
    Connotation.Value:=X;
    result:=GetNode(X);
end;


Procedure TNumboSlipnet.create_proximity_links;
var x,y,z: integer; 
begin
     for x:= 0 to 150 do
     begin
          for y:=-1 to 1 do
          begin
               z:= x+y;
               if (z>=0) and (z<=150) and (z<>x) then
                    Create_Link_Between_Numbers(x,z,1-(y*y/10));
          end;
     end;
end;

Procedure TSlipnet.Create_Link_Between_Nodes (NOrigin, NDest: Tnode; drag: real);
Var Link: Tlink;
begin
     Link:= Tlink.create (NDest, drag);
     NOrigin.RegisterObserver(Link);
end;





Procedure TNUMBOSLIPNET.Create_Link_Between_Numbers (num1, num2: integer; drag: real);
var N1, N2: TNumber;  Node1, Node2: TNode;
begin
    N1:=TNumber.create;
    N1.value:=Num1;
    N2.value:=Num2;

    Node1:=GetNode(n1);
    Node2:= GetNode(n2);
    Create_Link_Between_Nodes(Node1,Node2,drag);
end;


Procedure TNumboSlipnet.Create_SalientNode_Links;
     {Two kinds of salient nodes: the squares, and the multiples of 10: 130, 140, etc}
     {Creating multiples of 10}
     var x,z: integer;
begin
     {Creating SALIENT NUMBERS to MULTIPLES OF 10 LINKS}
     for x:= 0 to 150 do
     begin
          z:=x - (x mod 10);
          if (z>=0) and (z<=150) and (z<>x) then
               Create_Link_Between_Numbers (x, z, Salient_Multiples_10_drag);

          z:=z+10;
          if (z>=0) and (z<=150) and (z<>x) then
               Create_Link_Between_Numbers (x, z, Salient_Multiples_10_drag);
     end;

     {Creating Salient Numbers Links to Squares}
     {why not multiplication links?}
     for x:= 1 to 15 do
         Create_Multiplication_Decompositions (x*10);
end;



Procedure TNumboSlipnet.Create_Multiplication_Decompositions (x: integer);
var y: integer;
begin
     for y:= 1 to (x div 2) do
         if ((x mod y)=0) and (y<=(x div y)) then
         begin
              Create_Multiplication_Node(y, (x div y));
         end;
end;

procedure TNumboSlipnet.Create_Sum_Nodes_and_Links;
begin
{     for x:= 1 to 8 do                                      
     begin
          for y:= x to 9 do
          begin                                 
               TNumboSlipnet.Create_Sum_Node (x,y);
          end;}

     {For Jarbas's system, do it again for the salient numbers.  Link them to their decompositions.}

end;



Procedure TNumboSlipnet.Create_Multiplication_Nodes_and_Links;
var x, y: integer;
begin
     for x:= 2 to 14 do
     begin
          for y:= x to 14 do
          if (X*y<=150) then
               Create_Multiplication_Node(x,y);
     end;
end;



Procedure TNumboSlipnet.Create_Multiplication_Node(x,y: integer);
Var N1, N2, NResult: Tnode; N_MUltiple: TMultiplication_Node; L1, L2, L3: TLink;
begin
     N1:= GetNode(x);
     N2:= GetNode(y);
     NResult:=GetNode(x*y);
     N_Multiple:= TMultiplication_Node.create;
     N_Multiple.create_links(N1, N2, NResult);

     {Now for the hard part: Numbers also link to the multiplication!}
     L1:= Tlink.create(N_Multiple, high_drag_Multiplication_Node);
     N1.RegisterObserver(L1);

     L2:= Tlink.create(N_Multiple, high_drag_Multiplication_Node);
     N2.RegisterObserver(L2);

     L3:= Tlink.create(N_Multiple, high_drag_Multiplication_Node);
     NResult.RegisterObserver(L3);

     nodes.Add(N_multiple);
end;


Constructor TMultiplication_Node.Create_links(N1, N2, NResult: Tnode);
Var L_op1, L_op2, L_Res: Tlink;
begin
     {Criar 2 links para os operandos, e um link para o resultado}

     {Initially, create the multiplication node's associations (i.e., links)
      So now the Node is the origin, and its links point to the numbers's nodes}
     L_Op1:=Tlink.create (N1, drag_Multiplication_Node);
     L_Op2:=Tlink.create (N2, drag_Multiplication_Node);
     L_Res:=Tlink.create (NResult, drag_Multiplication_Node);
     RegisterObserver(L_Op1);
     RegisterObserver(L_Op2);
     RegisterObserver(L_res);
end;

end.
