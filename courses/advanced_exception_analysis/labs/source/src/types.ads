package Types is

   type Category_T is
     (Groceries, Clothing, Home_Goods, Sporting_Goods, Furniture);
   function Convert
     (Category : Category_T)
      return String;
   function Convert
     (Category : String)
      return Category_T;

end Types;
