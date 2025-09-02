package body Employee is

   procedure Set_Attribute
     (O     : in out Person_T;
      Value :        String) is
   begin
      null;
   end Set_Attribute;

   function Get_Attribute
     (O : Person_T)
      return String is
   begin
      return "";
   end Get_Attribute;

   procedure Print (O : Person_T) is
   begin
      null;
   end Print;

end Employee;
