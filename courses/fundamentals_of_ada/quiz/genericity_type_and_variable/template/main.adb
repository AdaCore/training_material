-- Which of the following piece(s) of code is (are) legal?

procedure Main is
   --$ begin question
   generic
      type Element_T is (<>);
      Last : in out Element_T;
   procedure Write (P : Element_T);

   Numeric        : Integer;
   Enumerated     : Boolean;
   Floating_Point : Float;
   --$ end question

   procedure Write (P : Element_T) is
   begin
      Last := P;
   end Write;

   --$ line cut
   procedure Write_A is new Write (Integer, Numeric);
   --$ line cut
   procedure Write_B is new Write (Boolean, Enumerated);
   --$ line cut
   procedure Write_C is new Write (Integer, Integer'Pos (Numeric));
   --$ line cut
   procedure Write_D is new Write (Float, Floating_Point);

   --$ begin answer
   -- A. Legal
   -- B. Legal
   -- C. The second generic parameter has to be a variable
   -- D. The first generic parameter has to be discrete
   --$ end answer

begin
   null;
end Main;
