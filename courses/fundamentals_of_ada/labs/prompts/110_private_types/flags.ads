with Colors;
package Flags is
   -- For a map, we need a key and an element type
   type Key_T is new integer; -- implement something smarter!
   type Map_Element_T is private;
   type Map_T is private;

   procedure Add (Map         : in out Map_T;
                  Key         :        Key_T;
                  Description :        Colors.Color_Set_T;
                  Success     :    out Boolean);
   procedure Remove (Map     : in out Map_T;
                     Key     :        Key_T;
                     Success :    out Boolean);
   procedure Modify (Map         : in out Map_T;
                     Key         :        Key_T;
                     Description :        Colors.Color_Set_T;
                     Success     :    out Boolean);

   function Exists (Map : Map_T; Key : Key_T) return Boolean;
   function Get (Map : Map_T; Key : Key_T) return Map_Element_T;
   function Image (Item : Map_Element_T) return String;
   function Image (Flag : Map_T) return String;
private
   -- Implement these types
   type Map_Element_T is null record;
   type Map_T is null record;
end Flags;
