with Swaps; use Swaps;
with Ada.Text_IO;
with Sorts; use Sorts;

procedure Main is
   V1 : Integer := 42;
   V2 : Integer := 43;
   T  : Integer_List := (2, 7, 1, 9, 40, -1);

begin
   Display_List (T);
   Sort(T);
   Display_List (T);

   Ada.Text_IO.Put_Line ("V1 :=" & Integer'Image (V1) &
                         " V2 :=" & Integer'Image (V2));
   Swap (V1, V2);
   Ada.Text_IO.Put_Line ("V1 :=" & Integer'Image (V1) &
                         " V2 :=" & Integer'Image (V2));
end Main;
