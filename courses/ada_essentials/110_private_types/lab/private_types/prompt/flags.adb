package body Flags is

   procedure Add
     (Map         : in out Map_T;
      Key         :        Key_T;
      Description :        Colors.Color_Set_T;
      Success     :    out Boolean) is
   begin
      Success := False;
      -- If the key is not already in the map then
      --    Create a map component and add it to the map
   end Add;

   procedure Remove
     (Map     : in out Map_T;
      Key     :        Key_T;
      Success :    out Boolean) is
   begin
      Success := False;
      -- Remove the component specified by the key from the map
   end Remove;

   procedure Modify
     (Map         : in out Map_T;
      Key         :        Key_T;
      Description :        Colors.Color_Set_T;
      Success     :    out Boolean) is
   begin
      Success := False;
      -- Update the component at the key location with the new data
   end Modify;

   function Exists
     (Map : Map_T;
      Key : Key_T)
      return Boolean is
   begin
      -- Return True if the key is in the map
      return False;
   end Exists;

   function Get
     (Map : Map_T;
      Key : Key_T)
      return Map_Component_T is
      Ret_Val : Map_Component_T;
   begin
      -- Return the map component specified by key
      return Ret_Val;
   end Get;

   function Image
     (Item : Map_Component_T)
      return String is
   begin
      -- return a string representation of the component
      return "";
   end Image;

   function Image
     (Flag : Map_T)
      return String is
   begin
      -- return a string representation of the map
      return "";
   end Image;

end Flags;
