unit Activation1Unit;

interface
                                                
uses sysutils, math;

const max_steps = 500; amplification = 1.2;   
                                             
type
         IActivation_Strategy = Interface
                                          function Recompute_Activation(Current_state: Real):Real;
                                 end;

         TRecompute_Activation_Sigmoid = Class (TInterfacedObject, IActivation_Strategy)
                                               function Recompute_Activation(Current_state: Real):Real;
                                         end;

         TRecompute_Activation_Linear = Class (TInterfacedObject, IActivation_Strategy)
                                               function Recompute_Activation(Current_state: Real):Real;
                                         end;

         IDecay_Strategy = Interface
                                     function Recompute_Decay(state: Real):Real;
                                 end;

         TDecay_Sigmoid = Class (TInterfacedObject, IDecay_Strategy)
                                               function Recompute_Decay(state: Real):Real;
                                         end;

         TDecay_Linear = Class (TInterfacedObject, IDecay_Strategy)
                                               function Recompute_Decay(state: Real):Real;
                                         end;


         Tactivation = class
                          private
                            current_state, level, increment: real;
                            Signals_Received: real;
                            Decay_strategy: IDecay_Strategy;
                            Activation_Strategy: IActivation_Strategy;
                            
                          public


                            Procedure set_decay_sigmoid;
                            Procedure set_decay_linear;

                            Procedure set_activation_sigmoid;
                            Procedure set_activation_linear;
                            Constructor Create;
                            procedure increase (step: real);
                            Function Get_Level:real;
                            function Get_CurrentState:real;
                            function Get_Increment: real;
                            Procedure Reset_Increment;
                            Procedure DeployChange;
                            procedure decay;
                       end;

implementation





function TDecay_Sigmoid.Recompute_Decay(State:Real):real;
var  pyramid, max, t:real;  aux,counter: integer;
begin
     Max:=1;
     aux:=floor(State*max_Steps);
     for counter:= max_steps downto aux do
     begin
          t:= counter/max_steps;
          If(t<=0.5) then Pyramid:=1-t else pyramid:=t;
          Max:=Max-(4*Pyramid/max_steps);
     end;
     Result:= (1-max)/max_steps;
end;

function TDecay_Linear.Recompute_Decay(State:Real):real;
begin
     Result:= 1/max_steps;
end;

Procedure Tactivation.set_decay_sigmoid;
begin
     Decay_Strategy:= TDecay_Sigmoid.create;
end;

Procedure Tactivation.set_decay_linear;
begin
     Decay_Strategy:= TDecay_Linear.create;
end;

function TRecompute_Activation_Sigmoid.Recompute_Activation(Current_State:Real):real;
var pyramid, sum, t:real;  counter: integer;
begin
     Sum:=0;
     for counter:= 0 to floor (Current_State*max_Steps) do
     begin
          t:= counter/max_steps;
          If(t<=0.5) then Pyramid:=t else pyramid :=1-t;
          Sum:=(4*(1/max_steps)* Pyramid) + Sum;
     end;
     Result:= Sum;
end;


function TRecompute_Activation_Linear.Recompute_Activation(Current_State:Real):real;
begin
     Result:= Current_State;
end;


Procedure Tactivation.set_activation_sigmoid;
begin
     Activation_Strategy:= TRecompute_Activation_Sigmoid.create;
end;

Procedure Tactivation.set_activation_linear;
begin
     Activation_Strategy:= TRecompute_Activation_Linear.create;
end;


Constructor Tactivation.Create;
begin
     current_state:=0;
     Reset_Increment;
     increase(0);
     set_activation_sigmoid;
     set_decay_sigmoid;
end;

Procedure Tactivation.Reset_Increment;
begin
     increment:=0;
     signals_Received:=1;
end;

function TActivation.Get_CurrentState:real;
begin
     result:=current_state;
end;

function Tactivation.Get_Increment: real;
begin
     result:=increment;
end;


Procedure TActivation.DeployChange;
begin
     if (current_state+increment>1) then increment:=1-current_state;
     If (Increment>0) then
     begin
          Current_State:=Current_State+increment;
          If (Current_State>1) then Current_State:=1;
          Level:=Activation_Strategy.Recompute_Activation(Current_State);
     end;
     Reset_Increment;

     {if Level>0.5 then set_activation_linear; just testing on-the-fly change of algorithm, enabled by the strategy pattern}
end;

Procedure Tactivation.increase(step:real);
begin
     increment:=increment+step*Signals_Received;
     if increment>1 then increment:=1;
     Signals_Received:=(Signals_Received*amplification);
end;


Procedure Tactivation.decay;
begin
     {decays here...}
     Current_state:=Current_state-(decay_strategy.Recompute_Decay(Current_state));

     {dies out if and when too low}
     if (current_state<(1/max_steps)) then current_state:=0;

     {recomputes the activation level}
     Level:=Activation_Strategy.Recompute_Activation(Current_State);
end;



function TActivation.Get_Level:real;
begin
     Level:=Activation_Strategy.Recompute_Activation(Current_State);
     Result:=level;
end;


end.
