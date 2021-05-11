
with Base_Types;
with Rectangle;
procedure Main is

   Object : Rectangle.Object_T;
   Line1  : constant Base_Types.Line_T := ((1, 1), (1, 10));
   Line2  : constant Base_Types.Line_T := ((6, 6), (6, 15));
   Line3  : constant Base_Types.Line_T := ((1, 1), (6, 6));
   Line4  : constant Base_Types.Line_T := ((1, 10), (6, 15));
begin
   Object.Set_Lines ((Line1, Line2, Line3, Line4));
   Object.Print;
end Main;
