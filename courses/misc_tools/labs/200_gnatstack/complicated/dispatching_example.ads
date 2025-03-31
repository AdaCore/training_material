package Dispatching_Example is

   type Primitive_T is tagged record
      Field : Boolean;
   end record;
   procedure Primitive_One (Param : in out Primitive_T);
   procedure Primitive_Two (Param : in out Primitive_T);

   type Derived_T is new Primitive_T with null record;
   overriding procedure Primitive_Two (Param : in out Derived_T);

   procedure Calls;

end Dispatching_Example;
