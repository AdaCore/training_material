procedure Main is
   -- This procedure is using top-notch algorithms to calculate info.
   -- It will perform a complex calculation (out of this exercice scope),
   -- using data you provide, to update a state register.
   -- Your job is to declare the data and set it to the right value, at the
   -- right time.

   -- QUESTION 1
   --
   -- Declare Running_Calculation of type Boolean, set to False

   -- Declare Active_Processors, a Positive set to 1

   -- Declare All_Active_Processors, set to the value of Active_Processors

   -- Declare Distance_Meters, a Float with a value of 2

   -- Declare Body_Weight_Kg, a *named number* with a value of 2.5 x 10^3

   -- Declare State_Register, a Natural with a binary value of 1 0101 1010

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

   -- QUESTION 3 - Quiz
   --
   -- After Calculate, what is the value of Running_Calculation?
   -- What is the value of All_Active_Processors?
   -- What is the value of Active_Processors?
   --
   -- QUESTION 3 - Part B
   --
   -- Update them to reflect the fact that we stopped calculating
   -- and are now using only one processor.

    -- Null statement to compile even without anything
    -- You can remove it at the end of the lab.
    null;
end Main;

-- You can use the 'Scenario' tab on the right to change the Mode from
-- problem to solution, click the checkmark button, and go to the sol
-- directory to compare your solution with the correction.
