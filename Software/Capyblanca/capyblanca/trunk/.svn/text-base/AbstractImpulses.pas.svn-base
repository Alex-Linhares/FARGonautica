unit AbstractImpulses;

interface
uses ExternalMemoryUnit, WorkingMemoryUnit,  BasicImpulse, BoardViewerUnit, classes, stdctrls, graphics;

type
     TRoles = (NoRole, Guardian, Attacker, Blocker, Interceptor);

     PTParamAbstractRole= ^TParamAbstractRole;

     TParamAbstractRole = class (TParameterSet)
                           Trajectory: TList;
                           Origin: TPiece;
                           Destination: TPiece;
                           intensity, v1, v2, Proximity: integer; {use 1 to 10000}
                           Roletype: TRoles;
                           Rolename: string;
                           DestinationSquare: Tsquare;
                           Intercepts: TParamAbstractRole;
                           Attack_Chain: TList;

                           
                     end;

     TImpulseAbstractRole = class (TImpulse)
                                  Parameters:TParamAbstractRole;

                                  Constructor create (P:TParamAbstractRole; U: TUrgency);
                                  Function GetAbstractRoleName:string;
                                  function InsertNewAbstractRoleByIntensity:integer;
                            end;

     TImpulseAttackerRole = class (TImpulseAbstractRole)
                                  Constructor create (P:TParamAbstractRole; U: TUrgency);
                                  procedure Fire; Override;
                                  end;

     TImpulseGuardianRole = class (TImpulseAbstractRole)
                                  Constructor create (P:TParamAbstractRole; U: TUrgency);
                                  procedure Fire; Override;
                                  end;

     TImpulsePromotionRole = class (TImpulseAbstractRole)
                                  Constructor create (P:TParamAbstractRole; U: TUrgency);
                                  procedure Fire; Override;
                                  end;

     TImpulseBlockerRole = class (TImpulseAbstractRole)
                                  Constructor create (P:TParamAbstractRole; U: TUrgency);
                                  procedure Fire; Override;
                                  end;

     TImpulseCheckInterception = class (TImpulseAbstractRole)
                                       NumberOfMoves: integer;

                                       enemycolor:boolean;
                                       pc: tpiece;
                                       Current_Square, squ, sqaux:^tsquare;
                                       x,y,x1,y1,x2,t1, aux:integer;
                                       s:string;
                                       Traj1: ^TList;
                                       NewInterceptionRole:^TParamAbstractRole;
                                       Impulse: ^TImpulseCheckInterception;
                                       disablerecursion:Boolean;

                                       Procedure Activate_Interceptor_Node (NewIntRole: PTParamAbstractRole);
                                       Constructor create (P:TParamAbstractRole; U: TUrgency);
                                       Procedure Check_Mobility_of_each_enemy_Piece_to_square;

                                       procedure fire; override;
                                 end;

    Function GetDest(P:TParamAbstractRole):TSquare;                     

    implementation
uses MainUnit, SpecialFunctions, Math,  Insert_delete_move_Impulses, impulseconsiderations, slipnetunit;


Function GetDest(P:TParamAbstractRole):TSquare;
begin
     if (P.Destination.sq.x>=1) and (P.Destination.sq.y>=1) and (P.Destination.sq.x<=8) and (P.Destination.sq.y<=8) then
     begin
          if (Workingmemory.GetpieceAt(P.Destination.sq)=nil) then result:=P.Destinationsquare;
     end
     else result:=P.Destination.sq;
end;

function StringTrajectory (Traj1:Tlist):string;
var count: integer;          sqaux: ^tsquare; s:string;
begin
     count:=0;
     sqaux:=Traj1.items[count];
     S:=S+ConvertSquare(sqaux^.x,sqaux^.y);
     while (count<traj1.count-1) do
     begin
          count:=count+1;
          sqaux:=Traj1.items[count];
          S:=S+', '+ConvertSquare(sqaux^.x,sqaux^.y);
     end;
     S:=S+']';
     result:= s;
end;


Constructor TImpulseAbstractRole.create (P:TParamAbstractRole; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

Function TImpulseAbstractRole.GetAbstractRoleName:string;   {THIS IS DOING NOTHING AND SHOULD BE DELETED OR REDESIGNED TO FIT SOMEPLACE OTHER THAN TImpulseAbstractRole.}
begin
     With Parameters do
     begin

     Case Roletype of
          guardian: rolename:='guardian';
          attacker: rolename:='attacker';
          blocker: rolename:='blocker';
          interceptor: rolename:='interceptor';
     end;
     result:= rolename;
     end; {with parameters}
end;

function TImpulseAbstractRole.InsertNewAbstractRoleByIntensity:integer;
var x1: integer;
    auxrole: TParamAbstractRole;
begin
     With Parameters do
     begin
     x1:=0;
     {inserts the abstract role by order of intensity}
     if (Origin.AbstractRoles.count=0) then
        Origin.AbstractRoles.add(Parameters)
     else begin
               x1:=0;
               while (x1<Origin.AbstractRoles.count) do
               begin
                    auxrole:=Origin.AbstractRoles.items[x1];
                    if (intensity>=auxrole.intensity) then
                    begin
                         Origin.AbstractRoles.insert(x1,Parameters);
                         x1:=Origin.AbstractRoles.count;
                    end;
                    x1:=x1+1;
               end;
          end;
     end;
     result:=x1;
end;


{===============================================================================}
Constructor TImpulsePromotionRole.create (P:TParamAbstractRole; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

procedure TImpulsePromotionRole.Fire;
var  s:string; Impulse:^TImpulseAbstractRole; index:integer;
Param: ^TParamAbstractRole;
begin
     With Parameters do
     begin
     Roletype:= Attacker;
     Rolename:= 'Promotion Attacker';

     {First measure of intensity, among other things}
     {intensity is a function of the value of the pieces and of the distances involved}
     {Traj^.pack;}
     Proximity:= 63 - Trajectory.count;
     if (proximity<=0) then Proximity:=1;

     v1:= (101-Origin.DynamicValue);
     v2:= QUEENVALUE;

     intensity:= (v1*v2*Proximity);
     str (Intensity, S);
     pmemo.lines.add('I see that the '+Origin.fullId +' might promote to '+Convertsquare(DestinationSquare.x,DestinationSquare.y));
     pmemo.lines.add('In principle, I find the intensity of this relation to be around '+s);

     Attack_Chain:=TList.create;
     Attack_Chain.add(Origin);

     {inserts the abstract role by order of intensity-- refactoring ?}
     index:=InsertNewAbstractRoleByIntensity;

     if (Intensity>100000) then
     begin
          {Launch interceptor-search}
          new (Impulse);
          new (Param);
          Param^:= TParamAbstractRole.create;
          Param.Trajectory:=trajectory;
          Param.intensity:=intensity;
          Param.Attack_Chain:=Attack_Chain;
          Param.Origin:=origin;
          Param.Intercepts:=Origin.AbstractRoles.Items[index];
          Param.pmemo:=pmemo;
          param.impulsepointer:=impulse;
          Impulse^:= TImpulseCheckInterception.create (Param^, 100);
     end;
     end; {-->With Parameters^ do}
end;


Constructor TImpulseCheckInterception.create (P:TParamAbstractRole; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;


Procedure TImpulseCheckInterception.Activate_Interceptor_Node (NewIntRole: PTParamAbstractRole);
var node: ^Tnode;   Param:TParamAbstractRole;
begin
     with Parameters do
     begin
          new (node);
          Param:= TParamGuardianConsiderations.create;
          Param.Trajectory:=trajectory;
          Param.Attack_Chain:=Attack_Chain;
          Param.Origin:=origin;
          Param.Intercepts:=NewIntRole^;
          Param.pmemo:=pmemo;
          Param.impulsepointer:=node;
          Node^:=tinterceptorNode.create(Param);
          Node.Activate(6000{Intensity*Intensity});
     end;
end;



procedure TImpulseCheckInterception.fire;
var step_in_trajectory: integer;
    t3, piece_index:integer;
    Node: ^Tnode;    Param: TParamAbstractRole;

begin
     With Parameters do
     begin
     Roletype:= Interceptor;
     Rolename:= 'Interceptor';
     {We must check if the attack from piece abstractrole.origin can be intercepted by any enemy piece}
     {this is done by checking the mobility of EACH enemy piece to EACH square in the attack trajectory}

     {for EACH square in the trajectory, starting from the most distant one... }
     numberofmoves:=(trajectory.count)-1;
     for step_in_trajectory:= Trajectory.count-1 downto 0 do
     begin
          enemycolor:= not (Origin.white);
          Current_Square:=Trajectory.items[step_in_trajectory];

          {Check_Mobility_of_each_enemy_Piece_to_square;}

          {check the mobility of EACH enemy piece to square sq!}
          for piece_index:=0 to workingmemory.pieces.count-1 do
          begin
               {get the proper square...}
               pc:=Workingmemory.Pieces.items[piece_index];

               disablerecursion:=false;
               for t3:= Attack_Chain.Count-1 downto 0 do  {check for interception circularity...}
                    if (pc=Attack_Chain.items[t3]) then disablerecursion:=true;

               if (pc.white=enemycolor) and (Disablerecursion=false) then
               begin
                   aux:=Pc.Mobility[Current_Square^.x,Current_Square^.Y].distance;
                   if (aux<0) then aux:=-aux;
                   if (aux<=numberofmoves) then
                   begin
                        str (numberofmoves,S);
                        pmemo.lines.add ('       The '+ Pc.FullId + ' may intercept an attack in '
                                           +s+' moves!');

                        {1st step: create interception trajectory}
                        new (Traj1);
                        Traj1^:=Tlist.create;
                        Traj1^:=PC.TrajectoryFinder(Current_Square^);

                        {2nd step: fill the data for the abstract role}
                        New (NewInterceptionRole);
                        NewInterceptionRole^:=TParamGuardianConsiderations.Create;
                        NewInterceptionRole.Roletype:=Interceptor;
                        NewInterceptionRole.Rolename:='Interceptor';
                        NewInterceptionRole.Origin:= Pc;
                        NewInterceptionRole.Trajectory:=Traj1^;
                        NewInterceptionRole.Attack_Chain:=TList.create;
                        for t3:= 0 to Attack_chain.count-1 do
                            NewInterceptionRole.Attack_Chain.add(Attack_chain.items[t3]);
                        {we've included the past...}
                        NewInterceptionRole.Attack_Chain.add(pc);

                        {3rd step: save link to original trajectory now intercepted,
                         and THEN calculate intensity of interception role}
                        NewInterceptionRole.Intercepts:=Parameters;
                        NewInterceptionRole.intensity:=Parameters.intensity;

                        str (NewinterceptionRole.intensity, S);
                        pmemo.lines.add ('       Intensity of interception seems to be around '+ S);
                        pmemo.lines.add ('       The trajectory is ['+StringTrajectory(Traj1^));

                        {&&& testing}
                        PARAMETERS:=NEWINTERCEPTIONROLE^;
                        InsertNewAbstractRoleByIntensity;
                        NEW (Impulse);
                        NewInterceptionRole.impulsepointer:=impulse;
                        Impulse^:=TImpulseCheckInterception.create(NewinterceptionRole^, 100);

                        {6th step create "interceptor" node}
                        {Activate_Interceptor_Node (NewInterceptionRole); REFACTOR... WHY CHANGES BEHAVIOR?}

                        (*
                        *)
                        new (node);
                        {new (Param);}
                        Param:= TParamGuardianConsiderations.create;
                        Param.Trajectory:=trajectory;
                        Param.Attack_Chain:=Attack_Chain;
                        Param.Origin:=origin;
                        Param.Intercepts:=NewInterceptionRole^;
                        Param.pmemo:=pmemo;
                        Param.impulsepointer:=node;
                        Node^:=tinterceptorNode.create(Param);
                        Node.Activate(6000{Intensity*Intensity});
                        (*
                        *)
                    end;
               end;
          end;
(**)

          numberofmoves:=numberofmoves-1;
     end;
     end; {-->With Parameters^ do}
end;



Constructor TImpulseAttackerRole.create (P:TParamAbstractRole; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

procedure TImpulseAttackerRole.Fire;
var    s:string;
begin
     With Parameters do
     begin
          if (Origin.trajectory_is_safe(destination.sq)) and (Origin.Mobility[destination.sq.x, destination.sq.Y].distance<>11000) then
          begin
               Roletype:= Attacker;

               {First measure of intensity, among other things}
               {intensity is a function of the value of the pieces and of the distances involved}
               Proximity:= 63 - Trajectory.count;
               if proximity<0 then Proximity:=1;

               v1:= (101-Origin.DynamicValue);
               v2:= (Destination.DynamicValue);
               proximity:= proximity;


               intensity:= (v1*v2*Proximity);
               str (Intensity, S);
               pmemo.lines.add('I see that the '+Origin.fullId +' might attack the '+Destination.FullID);
               pmemo.lines.add('In principle, I find the intensity of this relation to be around '+s);

               {inserts the abstract role by order of intensity}
               InsertNewAbstractRoleByIntensity;

               {Trigger impulse to check if this attack can be stopped}
         end;
     end; {-->With Parameters^ do}
end;                                                


Constructor TImpulseGuardianRole.create (P:TParamAbstractRole; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

procedure TImpulseGuardianRole.Fire;                         
var  s:string; Node: ^Tnode; 
begin
     With Parameters do
     begin
     if (Origin.isDefending(Destination.sq)=false) and (Origin.Mobility[destination.sq.X,destination.sq.y].distance<>11000) then
     begin
     Roletype:= Guardian;
     Rolename:='Guardian';

     {First measure of intensity, among other things}
     {intensity is a function of the value of the pieces and of the distances involved}

     Proximity:= 63 - (Trajectory.count);
     if proximity<0 then Proximity:=1;

     v1:= (101-Origin.DynamicValue);
     v2:= (Destination.DynamicValue);

     intensity:= v1*v2*Proximity;

     str (Intensity, S);
     pmemo.lines.add('I see that the '+Origin.fullId +' is a guardian of the '+Destination.FullID);
     pmemo.lines.add('In principle, I find the intensity of this relation to be around '+s);

     {inserts the abstract role by order of intensity}
     InsertNewAbstractRoleByIntensity;

     {ACTIVATE GUARDIAN ROLE NODE}
     New(node);
     Node^:=TGuardianNode.create(Parameters);  
     Node.activate(Intensity);

     end; {if}
     end;{-->With Parameters^ do}
end;


Constructor TImpulseBlockerRole.create (P:TParamAbstractRole; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

procedure TImpulseBlockerRole.Fire;
begin
     With Parameters do
     begin
     Roletype:= Blocker;
     Rolename:= 'Blocker';
     Intensity:=50;
     end; {-->With Parameters^ do}
end;

Procedure TImpulseCheckInterception.Check_Mobility_of_each_enemy_Piece_to_square;
begin
(**)
end;


end.



























