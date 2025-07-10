with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   type Days_Of_Week_T is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   --  type Unconstrained_Array_T is ?
   --  Constant_Array : ?
   --  Array2 : ?

   --  type Name_T is ? (array of characters)
   --  Weekly_Staff : ? (array if an array of Name_T 
   --                    day and counter should be array indexes)

begin
   Put_Line ("Array Types");
   -- Copy Constant_Array to Array2
   -- For every item of Array2
      -- Print the item

   -- Initialize Array2 via aggregate
   -- For every item of Array2
      -- Print the array index and the item

   -- Copy part of Constant_Array to part of Array2
   -- Set another part of Array2 to a known value
   -- For every item of Array2
      -- Print the item

   -- Initialize Weekly_Staff using nested aggregates
   -- For every day of the week
      -- Print the day of the week
      -- Print each staff member

end Main;
