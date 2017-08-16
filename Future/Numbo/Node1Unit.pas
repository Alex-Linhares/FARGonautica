unit Node1Unit;

interface

uses Activation1Unit, ObserverUnit, Classes, math;

type
         TActivationObserverClass = Class (TInterfacedObject, IObserverActivation)
                                          {Observer Interface here}
                                          procedure Update(Received_Activation: Real);  virtual;abstract;
                                    end;



         Tnode = class (TInterfacedObject, ISubjectActivation)
                 protected
                      named: string;

                 public
                      associations: tlist;
                      
                      activation: Tactivation;
                      constructor create;
                      Procedure SetName(S:String);
                      Function Name:string;
                      Function Number_of_Connotations: integer;
                      Function Get_Association_Numbered(X:integer):Tobject;

{                     Procedure Link_To(DestNode: TNode; Link: TLink; drag: real);}

                      {Observable (Subject) interface here}
                      procedure RegisterObserver(const Observer: TObject);
                      procedure UnRegisterObserver(const Observer: TObject);
                      procedure Notify (Sent_Activation: Tactivation);
                 end;



         TLink = Class (TActivationObserverClass)
                 protected
                      link_drag: real;
                      Dest_Node: TNode;

                 public
                      constructor create (destination: TNode; drag: real); overload;
                      constructor create (origin, destination: TNode; drag: real); overload;
                      Function Destination:Tnode;

                      {Observer Interface here}
                      procedure Update(Received_Activation: Real);  override;
                 end;


implementation

{TODO TNODE: {codelets: tlist;}{Codelet: ISubject; link_distance: real; link_Type: TApplinktype; }


Function TLink.Destination:Tnode;
begin
     result:= Dest_Node;
end;

constructor Tnode.create;
begin
     Activation:= Tactivation.Create;
     Associations:= tlist.Create;
     Named:='Number';
end;

procedure Tnode.RegisterObserver(const Observer: TObject);
Begin
     Associations.Add(Observer);
end;

Procedure TNode.SetName(s:string);
begin
     Named:=S;
end;

Function TNode.Name:string;
begin
     Result:=Named;
end;

Function TNode.Number_of_Connotations:integer;
begin
     result:=Associations.Count;
end;

Function Tnode.Get_Association_Numbered(X:integer):Tobject;
begin
     result:= associations.items[x];
end;

procedure TNode.UnRegisterObserver(const Observer: TObject);
var x:integer;
begin
     x:=Associations.IndexOf(Observer);
     if x>=0 then Associations.Delete(x);
end;


procedure TNode.Notify (Sent_Activation: TActivation);
var  i: Integer; x: Tlink;
begin
     for i := 0 to Associations.Count-1 do
     begin
          x:= Associations.Items[i];
          X.Update(Activation.Get_Increment/({2*}associations.Count));
     end;
end;                                       

constructor TLink.create (destination: TNode; drag: real);
begin
     Link_drag:= drag;
     Dest_Node:= destination;
end;

constructor Tlink.create (origin, destination: TNode; drag: real);
begin
     Link_drag:= drag;
     Dest_Node:=destination;
     Origin.RegisterObserver(self);                     
end;

Procedure Tlink.Update(Received_Activation: real);  {this method received the signal of activation propagation}
var step: real;
begin
     {I will implement directly here, but it should really be a strategy pattern,
     so this method should be refactored to strategy pattern to include other possibilities soon}

     step:= link_drag*Received_Activation;
     if (step>0) then
     begin
          Dest_node.activation.increase(step);
     end;
end;

end.