
with Colors;
package Flags is

   type Key_T is (Boston, London, Paris, Rome);

   type Map_Element_T is private;
   type Map_T is private;

   procedure Add
     (Map         : in out Map_T;
      Key         :        Key_T;
      Description :        Colors.Color_Set_T;
      Success     :    out Boolean);
   procedure Remove
     (Map     : in out Map_T;
      Key     :        Key_T;
      Success :    out Boolean);
   procedure Modify
     (Map         : in out Map_T;
      Key         :        Key_T;
      Description :        Colors.Color_Set_T;
      Success     :    out Boolean);
   function Exists
     (Map : Map_T;
      Key : Key_T)
      return Boolean;

   function Get
     (Map : Map_T;
      Key : Key_T)
      return Map_Element_T;

   function Image
     (Item : Map_Element_T)
      return String;

   function Image
     (Flag : Map_T)
      return String;

private
   type Map_Element_T is record
      Key         : Key_T;
      Description : Colors.Color_Set_T := Colors.Empty_Set;
   end record;
   type Map_Array_T is array (1 .. 100) of Map_Element_T;
   type Map_T is record
      Values : Map_Array_T;
      Length : Natural := 0;
   end record;

end Flags;
