package body Keys_Pkg is

   function In_Use return Natural is
     (0);

   function Generate return Key_T is
     ((others => <>));

   procedure Destroy (Key : Key_T) is
     null;

   function Image (Key : Key_T) return String is
     ("");

end Keys_Pkg;
