--Colors
package Colors is
   type Color_T is (Red, Yellow, Green, Blue, Black);
   type Color_Set_T is private;

   -- Add a color to the set
   procedure Add
     (Set   : in out Color_Set_T;
      Color :        Color_T);
   -- Remove a color from the set
   procedure Remove
     (Set   : in out Color_Set_T;
      Color :        Color_T);
   -- Convert the set to a string
   function Image
     (Set : Color_Set_T)
      return String;

private
   -- Implement Color_Set_T
   type Color_Set_T is null record;
end Colors;
