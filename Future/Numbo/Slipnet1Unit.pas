unit Slipnet1Unit;

{I DONT THINK THIS IS BEING USED, OR EVEN NECESSARY}


interface
         uses Activation1Unit, Node1Unit;

         type

         TLinkTypename = (proximity, multiplication, sum, landmark, subtraction, identity, result, decomposition);

         {TRULE =}

         TLinkType = class
                          rule: TLinkTypeName;
                          Activation: Tactivation; {???}

                          constructor create;                

                     end;

         TLink = class
                      constructor create;
                      procedure create_link (N1, N2: Tnode; rule:TLinkTypeName; distance: real);
                 end;




implementation




constructor TlinkType.create;
begin
     rule:=identity;

end;

constructor tlink.create;
begin

end;


procedure Tlink.create_link (N1, N2: Tnode; rule:TLinkTypeName; distance: real);
begin


end;

end.
