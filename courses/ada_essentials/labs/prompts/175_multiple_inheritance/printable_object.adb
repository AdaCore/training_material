package body Printable_Object is

   procedure Set_Attribute
     (Object    : in out Object_T;
      Attribute :        Integer) is null;

   function Get_Attribute
     (Object : Object_T)
      return Integer is (0);

end Printable_Object;
