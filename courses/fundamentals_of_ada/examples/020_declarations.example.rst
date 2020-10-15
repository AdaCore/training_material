-------------
Example Code
-------------

.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Declarations.Declarations_Example
    :class: ada-run

   procedure Declarations_Example is

      package Identifiers is
         Spaceperson : Integer;
         SPACEPERSON : integer; -- this identifier is illegal
         Space_Person : Integer;
         pragma Optimize (Off);
      end Identifiers;

      package Numeric_Literals is
         Feet_Per_Yard    : constant := 3;
         Quarter_Dollar   : constant := 0.25;
         Million_Dollars  : constant := 1_000_000.00;
         Escape_Character : constant := 8#033#;
         Ones_And_Zeroes  : constant := 16#AAAA#;
      end Numeric_Literals;

      package Object_Declarations is
         Distance, Speed : Float;
         Limit           : Integer := 200;
         Index           : Integer range 0 .. Limit;
      end Object_Declarations;

      package Named_Numbers is
         One_Third       : constant       := 1.0 / 3.0;
         Float_One_Third : constant Float := 1.0 / 3.0;
      end Named_Numbers;

      procedure Scope_And_Visibility is
         One : Integer;
      begin
         One := 1;
         declare
            One : Float := 2.3;
         begin
            One := One + Float (Scope_And_Visibility.One);
         end;
      end Scope_And_Visibility;

      package Aspect_Clauses is
         Eight_Bits : Integer range 0 .. 255 with
            Size => 8;
         Object : Integer with
            Atomic;
      end Aspect_Clauses;

   begin
      null;
   end Declarations_Example;

