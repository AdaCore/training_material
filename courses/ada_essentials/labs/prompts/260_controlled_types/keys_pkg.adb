package body Keys_Pkg is

   function Next_Available return Character is
   begin
      return ' ';
   end Next_Available;

   function In_Use return Natural is
   begin
      return 0;
   end In_Use;

   function Generate return Key_T is
      X : Key_T;
   begin
      return X;
   end Generate;

   procedure Destroy (Key : Key_T) is
   begin
      null;
   end Destroy;

   function Image
     (Key : Key_T)
      return String is
   begin
      return "";
   end Image;

end Keys_Pkg;
