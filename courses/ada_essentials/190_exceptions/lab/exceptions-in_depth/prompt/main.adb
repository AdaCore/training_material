with Ada.Text_IO; use Ada.Text_IO;
with Operations;

procedure Main is

   type Operation_T is record
      Operation : Operations.Operation_T;
      --  Add fields to store result and exception
      --  How do you know which field to use?
   end record;

   --  This array should test all four operations and
   --  generate the three exception cases we want to see.
   --  Remember if you add fields to Operation_T you will
   --  need to initialize them here as well
   Expressions : array (1 .. 7) of Operation_T :=
     (1 => (Operation => (1.1, '+', 2.2)),
      2 => (Operation => (33.3, '-', 44.4)),
      3 => (Operation => (555.5, '*', 666.6)),
      4 => (Operation => (77.7, '/', 88.8)),
      5 => (Operation => (9.9, '/', 0.0)),
      6 => (Operation => (9.9, '*', 8.8)),
      7 => (Operation => (7.7, '/', 0.06)));

begin

   --  Loop through all items in the array to run the operation
   --  and capture the success/failure

   --  Loop through all results and print the resulting value
   --  or the exception raised

   null;

end Main;
