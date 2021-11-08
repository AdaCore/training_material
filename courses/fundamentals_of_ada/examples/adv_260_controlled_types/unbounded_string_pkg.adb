with Ada.Unchecked_Deallocation;

package body Unbounded_String_Pkg is
   procedure Free_String is new Ada.Unchecked_Deallocation
     (String, String_Ref);

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

   procedure Finalize (Object : in out Ustring_T) is
   begin
      Free_String (Object.Ref);
   end Finalize;

   procedure Adjust (Object : in out Ustring_T) is
   begin
      Object.Ref := new String'(Object.Ref.all);
   end Adjust;
end Unbounded_String_Pkg;
