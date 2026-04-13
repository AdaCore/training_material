package body Employee is

   procedure Set_Attribute
     (This  : in out Person_T;
      Value :        String) is
   begin
      null;
   end Set_Attribute;

   function Get_Attribute
     (This : Person_T)
      return String is
   begin
      return "";
   end Get_Attribute;

   procedure Print (This : Person_T) is
   begin
      null;
   end Print;

end Employee;
