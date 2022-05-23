package Point_Of_Sale is

   procedure Catalog (With_Count : Boolean := False);

   procedure Sell_Item
     (Item  : String;
      Count : Positive := 1);

   procedure Return_Item
     (Item  : String;
      Count : Positive := 1);

end Point_Of_Sale;
