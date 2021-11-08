package Keys_Pkg is

   type Key_T is limited private;
   function Generate return Key_T;
   procedure Destroy (Key : Key_T);
   function In_Use return Natural;
   function Image (Key : Key_T) return String;

private
   type Key_T is limited null record;

end Keys_Pkg;
