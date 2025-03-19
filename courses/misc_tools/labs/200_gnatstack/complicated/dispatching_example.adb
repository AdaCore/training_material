package body Dispatching_Example is

   procedure Primitive_One (Param : in out Primitive_T) is
   begin
      Param.Field := not Param.Field;
      Primitive_Two (Param);
      Primitive_Two (Primitive_T'Class (Param));
   end Primitive_One;

   procedure Primitive_Two (Param : in out Primitive_T) is
   begin
      null;
      Param.Field := not Param.Field;
   end Primitive_Two;

   overriding procedure Primitive_Two (Param : in out Derived_T) is
   begin
      null;
      Param.Field := not Param.Field;
   end Primitive_Two;

   procedure Calls is
      One : Primitive_T;
      Two : Derived_T;
   begin
      Primitive_One (One);
      Primitive_One (Two);
   end Calls;

end Dispatching_Example;
