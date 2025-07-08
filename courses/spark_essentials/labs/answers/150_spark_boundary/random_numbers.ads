package Random_Numbers
with
  Abstract_State => Generator,
  Initializes => Generator
is

   procedure Random (From, To : Integer; Result : out Integer)
   with
     Global => (In_Out => Generator),
     Pre  => From <= To,
     Post => Result in From .. To;

end Random_Numbers;
