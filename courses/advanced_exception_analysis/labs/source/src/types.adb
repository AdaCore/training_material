with Ada.Characters.Handling; use Ada.Characters.Handling;
package body Types is

   function Convert
     (Category : Category_T)
      return String is
      Retval : String := To_Lower (Category'image);
   begin
      Retval (1) := To_Upper (Retval (1));
      return Retval;
   end Convert;

   function Convert
     (Category : String)
      return Category_T is (Category_T'value (Category));

end Types;
