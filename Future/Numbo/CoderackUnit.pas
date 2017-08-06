unit CoderackUnit;

interface

uses HedonicUnit, RelativeProbabilityUnit;

type
Tcoderack= class
                Current_Command: ICommand;
                RoulletteWheel: real;
                act: THedonic_Macrocommand;

                Constructor Create;
                Procedure CreateRoulletteWheel;
                Procedure Get_New_Command;
                Procedure select_Command(C:ICommand);
                Procedure ExecuteCommand;
            end;


implementation


Constructor TCoderack.Create;
begin
     RoulletteWheel:=0;
     Current_Command:= Repertoire.Commands.Items[random(repertoire.commands.count)];
     Act:=THedonic_Macrocommand.create;
end;

Procedure TCoderack.CreateRoulletteWheel;
var x:integer;  P:TRelative_Probability; C:ICommand;
begin
     RoulletteWheel:=0;
     {Sweeps the promising list first, then the repertoire}
     for x:= 0 to Current_Command.Promising_List.Count-1 do
     begin
          P:=Current_Command.Promising_Probability_List.items[x];
          RoulletteWheel:=RoulletteWheel+P.Get_Fitness;
     end;
     {NOW sweeps for the "missing" commands in the repertoire}
     for x:= 0 to Repertoire.Commands.Count-1 do
     begin
          C:=Repertoire.Commands.items[x];
          {If "missing", then include}
          if (Not Current_Command.HasInPromisingList(C)) then
          begin
               P:=c.Probability;
               RoulletteWheel:=RoulletteWheel+P.Get_Fitness;
          end;
     end;
end;


Procedure TCoderack.Get_New_Command;
var x:integer;  P:TRelative_Probability; C:ICommand; selection:real;
begin
     CreateRoulletteWheel;

     selection:= RoulletteWheel * random;
     {Sweeps the promising list first, then the repertoire}
     for x:= 0 to Current_Command.Promising_List.Count-1 do
     begin
          P:=Current_Command.Promising_Probability_List.items[x];
          Selection:=Selection-P.get_Fitness;
          if selection<=0 then {THIS ITEM IS SELECTED!}
          begin
               C:=Current_Command.Promising_List.items[x];
               select_Command(C);
               break;
          end;
     end;
     {NOW sweeps for the "missing" commands in the repertoire}
     if (selection>0) then
     begin
          for x:= 0 to Repertoire.Commands.Count-1 do
          begin
               C:=Repertoire.Commands.items[x];
               P:=c.Probability;
               {If "missing", then include}
               if (Not Current_Command.HasInPromisingList(C)) then
               begin
                  Selection:=Selection-P.get_Fitness;
                  if selection<=0 then {THIS ITEM IS SELECTED!}
                  begin
                       select_Command(C);
                       break;
                  end;
               end;
          end;
     end;
end;



Procedure TCoderack.ExecuteCommand;
begin
     Current_Command.execute;
{HERE???}  {   Act.ProcessFeedback (Current_Command.Wmem.Get_Feedback);       {}
end;



Procedure TCoderack.select_Command(C:ICommand);
begin
     {adds current command to "act"; act has size "MagicNumber", so it might delete the last one over there also}
     Act.Commands.Add(Current_Command);

     if Act.Commands.count>MagicNumber then
        Act.Commands.Delete(0);

{HERE???}     Act.ProcessFeedback (Current_Command.Wmem.Get_Feedback);{}
        
     Current_Command:=C;
end;


end.
