procedure Main is
   -- This procedure is using top-notch algorithms to calculate info.
   -- It will perform a complex calculation (out of this exercice scope),
   -- using data you provide, to update a state register.
   -- Your job is to declare the data and set it to the right value, at the
   -- right time.

   -- QUESTION 1
   --
   -- Declare Running_Calculation of type Boolean, set to False
   Running_Calculation : Boolean := False;
   -- Declare Active_Processors, a Positive set to 1
   Active_Processors : Positive := 1;
   -- Declare All_Active_Processors, set to the value of Active_Processors
   All_Active_Processors : Positive := Active_Processors;
   -- Declare Distance_Meters, a Float with a value of 2
   Distance_Meters : Float := 2.0;
   -- Declare Body_Weight_Kg, a *named number* with a value of 2.5 x 10^3
   Body_Weigth_Kg : constant := 2.5E+3;
   -- Declare State_Register, a Natural with a binary value of 1 0101 1010
   State_Register : Natural := 2#1_0101_1010#;
begin

   -- QUESTION 2 - Part A
   --
   -- Create a declarative block named Calculate.
   --
   -- In it declare a new variable Active_Processors, set it to 10
   --
   -- In Calculate body:
   --
   -- Set Running_Calculation to True
   --
   -- At this point your code should compile and run properly.
   --
   -- QUESTION 2 - Part B
   --
   -- Set All_Active_Processors to the sum of
   --  * the local Active_Processor
   --  * and Main.Active_Processor
   --
   -- QUESTION 2 - Part C
   --
   -- Use name qualifiers for both to override name hiding.
   Calculate : declare
      Active_Processors : Positive := 10;
   begin
      Running_Calculation := True;
      All_Active_Processors
        := Main.Active_Processors + Calculate.Active_Processors;
   end Calculate;

   -- QUESTION 3 - Quiz
   --
   -- After Calculate, what is the value of Running_Calculation?
   --    True
   -- What is the value of All_Active_Processors?
   --    11
   -- What is the value of Active_Processors?
   --    1
   --
   -- QUESTION 3 - Part B
   --
   -- Update them to reflect the fact that we stopped calculating
   -- and are now using only one processor.
   Running_Calculation := False;
   All_Active_Processors := Active_Processors;

end Main;
