with Swaps; use Swaps;
with Ada.Text_IO;
with Swap_Generics;
with Sort_Generics;

procedure Main is
   V1 : Integer := 42;
   V2 : Integer := 43;

   F1 : Float := 42.0;
   F2 : Float := 43.0;

   procedure Swap_Integer is new Swap_Generics.Swap_Generic (Integer);
   procedure Swap_Float is new Swap_Generics.Swap_Generic (Float);


   function To_String(I : Integer) return String is (Integer'Image(I));
 type Integer_List  is array (Integer range <>) of Integer;

   T  : Integer_List := (2, 7, 1, 9, 40, -1);


   package Sort_Integers is new Sort_Generics
     (Element_Type => Integer,
      List_Type    => Integer_List,
      Compare      => Standard."<",
      To_String => To_String);
   use Sort_Integers;

begin
   Display_List (T);
   Sort_Generic (T);
   Display_List (T);

   Ada.Text_IO.Put_Line ("V1 :=" & Integer'Image (V1) &
                         " V2 :=" & Integer'Image (V2));
   Swap (V1, V2);
   Swap_Integer (V1, V2);
   Ada.Text_IO.Put_Line ("V1 :=" & Integer'Image (V1) &
                         " V2 :=" & Integer'Image (V2));

   Ada.Text_IO.Put_Line ("F1 :=" & Float'Image (F1) &
                         " F2 :=" & Float'Image (F2));
   Swap_Float (F1, F2);
   Ada.Text_IO.Put_Line ("F1 :=" & Float'Image (F1) &
                         " F2 :=" & Float'Image (F2));
end Main;
