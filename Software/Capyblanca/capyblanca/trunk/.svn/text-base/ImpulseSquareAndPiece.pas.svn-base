unit ImpulseSquareAndPiece;

interface
                                               
Uses ExternalMemoryUnit, WorkingMemoryUnit, BasicImpulse, BoardViewerUnit, classes, stdctrls;

type
      TParamImpFindRelations = class (TParameterSet)
                                     p1, p2: Tpiece;
                               end;

      TImpulseFindRelations = class (TImpulse)
                                    Parameters: TParamImpFindRelations;
                                    Constructor create (P:TParamImpFindRelations; U: TUrgency);
                                    procedure Fire; Override;
                              end;


      TParamImpMakeTrajectory = class (TParameterSet)
                                     p1, p2: Tpiece;
                                     destination: tsquare;
                               end;

      TImpulseMakeTrajectory = class (TImpulse)
                                     Parameters:TParamImpMakeTrajectory;
                                     Constructor create (P:TParamImpMakeTrajectory; U: TUrgency);
                                     procedure Fire; Override;
                                     Procedure Relaunch;
                               end;

      TImpulseMakeTrajectoryToSquare = class (TimpulseMakeTrajectory)
                                  Constructor create (P:TParamImpMakeTrajectory; U: TUrgency);
                                  procedure Fire; Override;
                            end;

      TimpulseMakePawnTrajectory = class (TImpulseMakeTrajectory)
                                     Parameters:TParamImpMakeTrajectory;
                                     Constructor create (P:TParamImpMakeTrajectory; U: TUrgency);
                                     procedure Fire; Override;
                               end;

      TParamCheckSquare = class (TParameterSet)
                                  ProbNextOccurence: real;
                                  move: tsquare;
                                  L:Tlist;
                               end;

      TImpulseCheckSquare = class (TImpulse)
                                  Parameters: TParamCheckSquare;
                                  Constructor create (P:TParamCheckSquare; U: TUrgency);
                                  procedure Fire; Override;
                                  end;


      TParamFoundPiece = class (TParameterSet)
                                  Position: TSquare;
                                  PWhite: boolean;
                                  PieceType: TPieceType;
                                  newpiece:tpiece;
                                  CurrentFloodFillPos:Tsquare;
                          end;


      TImpulseFoundPiece = class (Timpulse)
                                  Parameters: TParamFoundPiece;
                                  Constructor create (P:TParamFoundPiece; U: TUrgency);
                                  procedure Fire;  override;
                                  function getnextmoves:Tlist; virtual; abstract;
                           end;

      TParamMapTerrain = class (TParameterSet)
                                 epicenter:tsquare;
                                 dist:integer;
                                 height: integer;
                                 Piece: Tpiece;
                                 NextMoves: Tlist;
                                 destination: tsquare;
                         end;

      TImpulseMapTerrain = class (TImpulse)
                                 Parameters: TParamMapTerrain;
                                 Constructor create (P:TParamMapTerrain; U: TUrgency);
                                 Procedure Find_Relations; virtual;
                                 procedure fire; override;
                           end;

      TImpulseMapTerrainUniqueRelation= class (TImpulseMapTerrain)
                                 Constructor create (P:TParamMapTerrain; U: TUrgency);
                                 Procedure Find_Relations; override;
                           end;

implementation
uses AbstractImpulses;

{=========================================================}
Constructor TImpulseCheckSquare.create (P:TParamCheckSquare; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

procedure TImpulseCheckSquare.Fire;
var x,y,t:integer; Impulse:^timpulse; ppos: ^Tsquare; s1,s2:string;
    potentialsquare: ^tsquare; CheckedSquare: Tpiece; PotentialShadows:Tlist;

    Param: ^TParamCheckSquare;  Param2: ^TParamFoundPiece;
begin
     with Parameters do
     begin
     urges.pack;
     L:=Tlist.create;
     x:= move.x;
     y:= move.y;

     pmemo.lines.add('Saccading to '+convertsquare(x,y)+'...');
     Workingmemory.Saccaded[x,y]:=true;

     if (ExternalMemory.square[x,y]<>none) and (ExternalMemory.found[x,y]=false) then
     begin
          {launch top-down urge to create piece with highest urgency}
          {(i) creates piece in working memory -- piece very unhappy }

          pmemo.lines.add('There´s something here at '+convertsquare(x,y)+'...');
          new (impulse);
          new(Param2);
          Param2^:=TParamFoundPiece.create;
          Param2.Position:=move;
          Param2.pmemo:=pmemo;
          Param2.PWhite:= ExternalMemory.white[x,y];
          Param2.PieceType:=ExternalMemory.square[x,y];
          Param2.impulsepointer:=impulse;

          impulse^:=TImpulseFoundPiece.create (Param2^, 250);
     end;

     ExternalMemory.found[x,y]:=true; {here or inside the if?}

     {NOW We must check shadows... Do we glance any shadows?}
      new (ppos);
      ppos^.x:=x;
      ppos^.y:=y;

      CheckedSquare:= TKing.create(Ppos^, true, king); {let`s use the king`s upcoming movements to check shadows}
      PotentialShadows:= CheckedSquare.GetNextMoves(Ppos^);
      t:=0;
      PotentialShadows.pack;
      while ((PotentialShadows.count>0) and (t<PotentialShadows.count))do
      begin
           potentialsquare:=PotentialShadows.items[t];

           if (externalmemory.square[potentialsquare^.x,potentialsquare^.y]<>none)
               and (not (externalmemory.found[potentialsquare^.x,potentialsquare^.y])) then
           begin
                str (x,s1);str(y,s2);
                pmemo.lines.add('...and I see a shadow at '+convertsquare(potentialsquare^.x,potentialsquare^.y));
                Workingmemory.GlancedShadow[potentialsquare^.x,potentialsquare^.y]:=true;

                New(impulse);
                new (Param);
                Param^:=TParamCheckSquare.Create;
                Param.move:=potentialsquare^;
                param.pmemo:=pMemo;
                param.impulsepointer:=impulse;

                Impulse^:=TImpulseCheckSquare.create(Param^, 250);
           end;
           t:=t+1;
      end;
      CheckedSquare.destroy;
      PotentialShadows.destroy;
      end; { with parameters}
end;

{=========================================================}
Constructor TImpulseMakeTrajectory.create (P:TParamImpMakeTrajectory; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

Procedure TImpulseMakeTrajectory.Relaunch;
var I:^Timpulse;
    P:^TParamMapTerrain;
begin
     new (P);
     P^:= TParamMapTerrain.Create;
     P.Piece:=Parameters.p1;
     P.destination:=parameters.p2.sq;
     P.pmemo:=Parameters.pmemo;
     new (I);
     p.impulsepointer:=i;
     I^:=TImpulseMapTerrainUniqueRelation.create(p^, 20);
end;


procedure TImpulseMakeTrajectory.Fire;
var t, index1, index2:integer; Paux1:tpiece;

begin
     with Parameters do
     begin
     {0. First of all, do both pieces still exist, or have they been killed?}
     index1:=-1; index2:=-1;
     for t:= 0 to workingmemory.pieces.count-1 do
     begin
          Paux1:=workingmemory.pieces.items[t];
          if (Paux1=p1) then index1:= t;
          if (Paux1=p2) then index2:= t;
     end;
     if (index1=-1) or (index2=-1) then
        exit;

     {1. We must scan the list of trajectories to know if there is already a trajectory there...}
     {scan (workingmemory) trajectories para achar tanto a P1 quanto a P2}

     if (index1>0) and (index2>0) then
     begin
          P1.DeleteAbstractRoles(p2.sq);             

          {2. Say something}
          pmemo.lines.add('creating trajectory from '+ P1.FullId +' to '+P2.fullid);

          P1.Launch_Impulse_Finding_Relation_To (P2, Pmemo);

          if (P1.Mobility[p2.sq.x,p2.sq.Y].distance<>11000) then
             if (P1.Trajectory_is_safe(p2.sq)=false) then
             begin
                  P1.DeleteabstractRoles(p2.sq);
                  pmemo.Lines.add('  --> Role of '+p1.FullId+' leading to '+p2.FullId+' has been deleted because trajectory is not safe ');
                  Relaunch; {-->RECHECK TRAJECTORY BASED ON NEW UPDATED MOBILITY; PRINT & STUDY FOR GOD SAKE}
             end;
     end;

     end; {-->with parameters^ do}

end;


{=========================================================}
Constructor TImpulseFindRelations.create (P:TParamImpFindRelations; U: TUrgency);
begin                        
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

procedure TImpulseFindRelations.Fire;
var t, index, column, row,x, x1 :integer;  i:^timpulse;
    s1,s_aux:Tsquare; s2:^tsquare; squares:tlist; piecepoint:^tpiece;   sqspaux1, sqspaux2:tsquare;

    Param: ^TParamImpMakeTrajectory; Param2: ^TParamImpMakeTrajectory; Param3: ^TParamImpMakeTrajectory;
begin
     With Parameters do
     begin
     {Will trigger RELEVANT trajectory building impulses, between reachable pieces, pawn chains, and promotions}
     {find index of P1}
     t:=0; index:=-1;
     while (t<workingmemory.pieces.count) do
     begin
          if (p1=workingmemory.pieces.items[t]) then index:=t;
          t:=t+1;
     end;
     if index=workingmemory.pieces.count then exit;   {woudn't it be T instead of index? and break instead of exit?}

     {first test if the target is equal to the origin...}
     new (piecepoint);
     sqspaux1:=p1.sq;
     x1:=0;
     repeat
           if (x1<>index) then
           begin
                piecepoint^:=workingmemory.Pieces.items[x1];
                sqspaux2:=Piecepoint^.sq;
                if (sqspaux1.x=sqspaux2.x) and (sqspaux1.y=sqspaux2.y) then
                   if (piecepoint^.white=P1.white) then
                   begin
                        exit;
                   end;
                end;
                x1:=x1+1;
     until (x1=workingmemory.Pieces.count);

     pmemo.Lines.add('Looking for relevant relations of '+P1.FullId);
     t:=0;
     while (t<workingmemory.pieces.count) do   {Checks relations with all other pieces}
     begin
          if (t<>index) then
          begin
               {new (p2);}
               P2:=workingmemory.pieces.items[t];

               if (p1.mobility[p2.sq.x,p2.sq.y].distance<>10000) then
               begin
                    {launch maketrajectory impulse between index and t}
                    new (Param);
                    Param^:=TParamImpMakeTrajectory.Create;
                    Param.p1:=p1;
                    Param.p2:=p2;
                    Param.pmemo:=pmemo;
                    new (i);
                    param.impulsepointer:=i;
                    I^:=TimpulseMakeTrajectory.create(Param^, 10);

               end;
               {JUST TESTING A THEORY HERE... Symetrical testing, just in case of alternate order of piece discovery}
               if (p2.mobility[p1.sq.x,p1.sq.y].distance<>10000) then
               begin
                    {launch maketrajectory impulse between index and t}
                    new (Param);
                    Param^:=TParamImpMakeTrajectory.Create;
                    Param.p1:=p2;
                    Param.p2:=p1;
                    Param.pmemo:=pmemo;
                    new (i);
                    param.impulsepointer:=i;
                    I^:=TimpulseMakeTrajectory.create(Param^, 10);

               end;
          end;
          t:=t+1;
     end;
     if (p1.piecetype = pawn) then {PAWN CHAINS AND PAWN PROMOTIONS IN HERE...}
     begin
          {Checks for pieces guarded by pawns chains}
{          squares:=tlist.create;}
          s1.x:=P1.sq.x;
          s1.y:=P1.sq.y;
          Squares:=P1.GetThreathenedSquares(s1);   {where could they be? which squares?}
          for x:= 0 to Squares.count-1 do
          begin
               {scans working memory for pieces in such position}
               s2:= squares.items[x];
               s_aux.x:= s2.x;
               s_aux.y:= s2.y;
               index:=-1;
               t:=0;
               while (t<Workingmemory.pieces.count-1)do
               begin
                    {new (P2);}
                    P2:=workingmemory.pieces.items[t];
                    if ( (p2.sq.x=s_aux.x) and (p2.sq.y=s_aux.y)) then index:=t;
                    t:=t+1;
               end;
               if (index>=0) then  {is there a piece in that position?}
               begin
                    {launch maketrajectory impulse between pawn p1 and piece p2}
                    p2:=workingmemory.pieces.items[index];

                    new(Param2);
                    Param2^:= TParamImpMakeTrajectory.Create;
                    Param2.p1:=p1;
                    Param2.p2:=p2;
                    new (i);
                    Param2.pmemo:=pmemo;
                    Param2.impulsepointer:=i;
                    I^:=TimpulseMakePawnTrajectory.create (Param2^, 25);
               end;
          end;
     end;

     if (p1.piecetype = pawn) then {Checks for pawn promotions}
     begin
          if P1.white then row:=1 else row:=8;
          for column:= 1 to 8 do
          begin
               if (p1.mobility[column,row].distance<>10000) then
               begin
                    {launch maketrajectory impulse between index and t}

                    new (Param3);
                    Param3^:=TParamImpMakeTrajectory.create;
                    Param3.P1:=P1;
                    Param3.destination.x:=column;
                    Param3.destination.y:=row;
                    Param3.pmemo:=pmemo;
                    new (i);
                    param3.impulsepointer:=i;
                    I^:=TimpulseMakeTrajectoryToSquare.create (Param3^, 25);
               end;
          end;
     end;
     end; {-->parameters}
end;

{=========================================================}
Constructor TImpulseFoundPiece.create (P:TParamFoundPiece; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

Procedure TImpulseFoundPiece.fire;
var s:string; height, dist: ^integer;  i:^timpulse;

Param: ^TParamMapTerrain;
begin
     With Parameters do
     begin

      { (i) creates piece in working memory -- piece very unhappy -- highest urgency=1000!}
      newpiece:=TPieceFactoryMethod(Position, pWhite, Piecetype);
      newpiece.Mobility[position.x,position.y].distance:=0;
      newpiece.sq.x:=position.x;
      newpiece.sq.y:=position.y;
      s:=NewPiece.FullId;
      s:=s+' ';
      pmemo.Lines.add('I just found a '+S+'.');

      new (height);
      height^:=0;
      new (dist);
      dist^:=0;

      WorkingMemory.Pieces.add(newpiece);
      {SAVES THE IMMEDIATE THREATS}
      WorkingMemory.Threatens(newpiece);

      new (Param);
      Param^:= TParamMapTerrain.Create;
      Param.Piece:=newpiece;
      Param.pmemo:=pmemo;
      new (I);
      param.impulsepointer:=i;
      I^:=TimpulseMapTerrain.create(param^, 10);
      end; {With parameters}
end;

{=========================================================}
Constructor TImpulseMapTerrain.create (P:TParamMapTerrain; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

Constructor TImpulseMapTerrainUniqueRelation.create (P:TParamMapTerrain; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;


procedure TImpulseMapTerrainUniqueRelation.Find_Relations;
var   Param:^TParamImpMakeTrajectory; I:^timpulse; s:string;
begin
      {Trigger impulses to find a specific relation--creating the trajectorie}
      new (Param);
      Param^:=TParamImpMakeTrajectory.Create;
      Param.p1:=parameters.Piece;
      Param.destination:=Parameters.destination;
      Param.pmemo:=parameters.pmemo;
      new (i);
      param.impulsepointer:=i;
      I^:=TimpulseMakeTrajectory.create(Param^, 10);
      s:=convertsquare(Parameters.destination.x, Parameters.destination.y);
      Parameters.pmemo.Lines.add('  --> Looking again at relation between '+parameters.piece.FullId+' & '+ S);

end;

Procedure TImpulseMapTerrain.Find_Relations;
var     DataImpFindRelations:^TParamImpFindRelations; I:^timpulse;
begin
      {go mapping each square in the following dimensions: (1) distance, (2) blockdness height}
      {Blockness=0 --> floodfill; Blockedness=1 --> at least one border crossing needed; etc}

      {Trigger impulses to find attack relations and find defense relations -- creating trajectories}
      New (DataImpFindRelations);
      DataImpFindRelations^:=TParamImpFindRelations.Create;
      DataImpFindRelations.p1:=Parameters.Piece;
      DataImpFindRelations.pmemo:=Parameters.pmemo;
      new (i);
      DataImpFindRelations.impulsepointer:=i;
      I^:=TimpulseFindRelations.create(DataImpFindRelations^, 50);
end;

Procedure TImpulseMapTerrain.fire;
var s:string;
begin
     With Parameters do
     begin
          piece.Map_Current_Terrain;
          Piece.computemobility;
          str (Piece.UnsafeMobility, S);
          pmemo.lines.add('mobility of '+piece.FullId+' is '+s);
          Find_Relations;
      end; {With parameters}
end;


{=========================================================}
Constructor TImpulseMakeTrajectoryToSquare.create (P:TParamImpMakeTrajectory; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

procedure TImpulseMakeTrajectoryToSquare.Fire;
begin
     With Parameters do
     begin
          pmemo.lines.add('creating trajectory from '+ P1.FullId +' to '+ConvertSquare(destination.x,destination.y));
          p1.Launch_Impulse_Finding_Relation_To(destination, pmemo);
     end; {With parameters...}
end;


{==============================================================================}
Constructor TImpulseMakePawnTrajectory.create (P:TParamImpMakeTrajectory; U: TUrgency);
begin
     Parameters:=P;
     Urgency:= U;
     Feel_Urging_Pressure (Parameters.impulsepointer);
end;

procedure TImpulseMakePawnTrajectory.Fire;
var Traj: Tlist; squ:^tsquare;

ParamImpAbsRole: ^TParamabstractRole; impulse: ^Timpulse;
begin
     With Parameters do
     begin
     {1. Get x,y of destination piece P2}
     pmemo.lines.add('creating trajectory from '+ P1.FullId +' to '+P2.fullid);


     {2.Connect next piece with p1}
     Traj:=Tlist.create;
     new (squ);
     squ^.x:=p2.sq.x;
     squ^.y:=p2.sq.y;
     Traj.insert(0,squ);
     new (squ);
     squ^.x:=p1.sq.x;
     squ^.y:=p1.sq.y;
     Traj.insert(0,squ);


{     P1.Launch_Impulse_Finding_Relation_To(P2, pmemo);}
     {3. REFACTORing NOT possible?}

     new (ParamImpAbsRole);
     ParamImpAbsRole^:=TParamabstractRole.Create;
     ParamImpAbsRole.Trajectory:=traj;
     ParamImpAbsRole.Origin:=P1;
     ParamImpAbsRole.Destination:=P2;
     ParamImpAbsRole.pmemo:=pmemo;

     New (impulse);
     paramimpabsrole.impulsepointer:=impulse;
     if (P1.white <> P2.white) then
         Impulse^:= TImpulseAttackerRole.create (ParamImpAbsRole^, 45)
     else
         Impulse^:= TImpulseGuardianRole.create (ParamImpAbsRole^, 45);

     end; {With parameters^ do}
end;


end.
