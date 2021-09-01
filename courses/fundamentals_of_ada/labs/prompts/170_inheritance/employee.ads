package Employee is

   type Person_T is tagged private;
   -- create primitive subprograms to set/get attributes for Person_T and
   -- to print the contents of Person_T
   procedure Set_Attribute
     (O     : in out Person_T;
      Value :        String);
   function Get_Attribute
     (O : Person_T)
      return String;
   procedure Print (O : Person_T);

   -- Create a new type Employee_T based on Person with some
   --    additional attributes
   -- Create primitive subprograms to set/get these new attributes and to
   -- print the entire contents of Employee_T

   -- Create a new type Position_T based on Person with some
   --    additional attributes
   -- Create primitive subprograms to set/get these new attributes and to
   -- print the entire contents of Position_T

private
   type Person_T is tagged null record;

end Employee;
