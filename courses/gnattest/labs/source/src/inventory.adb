with Ada.Containers.Ordered_Maps;

package body Inventory is

   use Ada.Strings.Unbounded;

   package Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Natural);
   use type Maps.Cursor;
   Database : Maps.Map;

   function Find
     (Key : String)
      return Maps.Cursor is (Maps.Find (Database, To_Unbounded_String (Key)));

   procedure Add
     (Item  : String;
      Count : Positive) is
      Cursor : Maps.Cursor;
      Key    : constant Unbounded_String := To_Unbounded_String (Item);
   begin
      Cursor := Find (Item);
      if Cursor = Maps.No_Element
      then
         Maps.Insert (Database, Key, Count);
      else
         declare
            New_Count : constant Natural := Maps.Element (Cursor) + Count;
         begin
            Maps.Replace (Database, Key, New_Count);
         end;
      end if;
   end Add;

   procedure Remove
     (Item  : String;
      Count : Positive) is
      Current   : constant Natural := Query (Item);
      To_Remove : constant Natural := Natural'min (Count, Current);
   begin
      if To_Remove > 0
      then
         Maps.Replace
           (Database, To_Unbounded_String (Item), Current - To_Remove);
      end if;
   end Remove;

   function Query
     (Item : String)
      return Natural is
      Cursor : constant Maps.Cursor := Find (Item);
   begin
      if Cursor = Maps.No_Element
      then
         return 0;
      else
         return Maps.Element (Cursor);
      end if;
   end Query;

   function List
     (With_Count : Boolean := False)
      return List_T is
      Retval : List_T (1 .. Integer (Maps.Length (Database)));
      Count  : Natural := 0;
      procedure Add (Cursor : Maps.Cursor) is
      begin
         Count          := Count + 1;
         Retval (Count) := Maps.Key (Cursor);
         if With_Count
         then
            Retval (Count) :=
              Retval (Count) & " (" & Maps.Element (Cursor)'Image & " )";
         end if;
      end Add;
   begin
      Maps.Iterate (Database, Add'Access);
      return Retval;
   end List;

end Inventory;
