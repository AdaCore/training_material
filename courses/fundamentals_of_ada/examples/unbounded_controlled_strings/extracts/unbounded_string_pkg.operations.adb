with Ada.Unchecked_Deallocation;

package body Unbounded_String_Pkg is

   function "=" (L, R : Ustring_T) return Boolean is
      ( L.Ref.all = R.Ref.all );

   function To_Ustring_T (Item : String) return Ustring_T is
      ( Controlled with Ref => new String'(Item) );

   function To_String (Item : Ustring_T) return String is
      ( Item.Ref.all );

   function Length (Item : Ustring_T) return Natural is
      ( Item.Ref.all'Length );

   function "&" (L, R : Ustring_T) return Ustring_T is
      (Controlled with Ref => new String'(L.Ref.all & R.Ref.all));
