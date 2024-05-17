with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Console;
with Inventory;

package body Point_Of_Sale is

   procedure Catalog (With_Count : Boolean := False) is
      List : Inventory.List_T := Inventory.List (With_Count);
   begin
      if List'Length = 0
      then
         Console.Print ("Inventory is completely empty");
      else
         Console.Print ("Inventory:");
         for Item of List
         loop
            Console.Print ("  " & To_String (Item));
         end loop;
      end if;
   end Catalog;

   procedure Sell_Item
     (Item  : String;
      Count : Positive := 1) is
   begin
      if Count > Inventory.Query (Item)
      then
         Console.Print ("Not enough " & Item & " in inventory");
      else
         Inventory.Remove (Item, Count);
         Console.Print ("Thank you for your purchase");
      end if;
   end Sell_Item;

   procedure Return_Item
     (Item  : String;
      Count : Positive := 1) is
   begin
      Inventory.Add (Item, Count);
      Console.Print ("Sorry you were not happy");
   end Return_Item;

end Point_Of_Sale;
