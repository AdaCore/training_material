with Ada.Strings.Unbounded;
package Inventory is

   procedure Add
     (Item  : String;
      Count : Positive);

   procedure Remove
     (Item  : String;
      Count : Positive);

   function Query
     (Item : String)
      return Natural;

   type List_T is
     array (Natural range <>) of Ada.Strings.Unbounded.Unbounded_String;
   function List
     (With_Count : Boolean := False)
      return List_T;

end Inventory;
