with Ada.Finalization; use Ada.Finalization;

package Unbounded_String_Pkg is
   type Ustring_T is private; --  Implementation

   function "=" (L, R : Ustring_T) return Boolean;
   function To_Ustring_T (Item : String) return Ustring_T;
   function To_String (Item : Ustring_T) return String;
   function Length (Item : Ustring_T) return Natural;
   function "&" (L, R : Ustring_T) return Ustring_T;
