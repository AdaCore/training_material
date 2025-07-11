.. code:: ada
    :class: ada-syntax-only

   package Tagged_Derivation is
   
      type Root_T is tagged record
         Root_Component : Integer;
      end record;
      function Primitive_1 (This : Root_T) return Integer is (This.Root_Component);
      function Primitive_2 (This : Root_T) return String is
        (Integer'Image (This.Root_Component));
   
      type Child_T is new Root_T with record
         Child_Component : Integer;
      end record;
      overriding function Primitive_2 (This : Child_T) return String is
        (Integer'Image (This.Root_Component) & " " &
         Integer'Image (This.Child_Component));
      function Primitive_3 (This : Child_T) return Integer is
        (This.Root_Component + This.Child_Component);
   
      -- type Simple_Deriviation_T is new Child_T; -- illegal
   
      type Root2_T is tagged record
         Root_Component : Integer;
      end record;
      -- procedure Primitive_4 (X : Root_T; Y : Root2_T); -- illegal
   
   end Tagged_Derivation;

   with Ada.Text_IO;       use Ada.Text_IO;
   with Tagged_Derivation; use Tagged_Derivation;
   procedure Test_Tagged_Derivation is
      Root  : Root_T  := (Root_Component => 1);
      Child : Child_T := (Root_Component => 11, Child_Component => 22);
   begin
      Put_Line ("Root: " & Primitive_2 (Root));
      Put_Line ("Child: " & Primitive_2 (Child));
      Root := Root_T (Child);
      Put_Line ("Root from Child: " & Primitive_2 (Root));
      -- Child := Child_T (Root); -- illegal
      -- Put_Line ("Child from Root: " & Primitive_2 (Child)); -- illegal
      Child := (Root with Child_Component => 999);
      Put_Line ("Child from Root via aggregate: " & Primitive_2 (Child));
   end Test_Tagged_Derivation;
