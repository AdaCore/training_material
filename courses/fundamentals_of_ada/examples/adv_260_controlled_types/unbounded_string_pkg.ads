with Ada.Finalization; use Ada.Finalization;
package Unbounded_String_Pkg is
   -- Implement unbounded strings
   type Ustring_T is private;
   function "=" (L, R : Ustring_T) return Boolean;
   function To_Ustring_T (Item : String) return Ustring_T;
   function To_String (Item : Ustring_T) return String;
   function Length (Item : Ustring_T) return Natural;
   function "&" (L, R : Ustring_T) return Ustring_T;
private
   type String_Ref is access String;
   type Ustring_T is new Controlled with record
      Ref : String_Ref := new String (1 .. 0);
   end record;
   procedure Finalize (Object : in out Ustring_T);
   procedure Adjust (Object : in out Ustring_T);
end Unbounded_String_Pkg;
