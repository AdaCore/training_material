--Database_Body_1
with Ada.Containers.Ordered_Maps;

package body City_Trivia is
   use type Information_List.Vector;
   package Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => City_Name_T,
        Element_Type => Information_List.Vector);
   use type Maps.Cursor;
   Map : Maps.Map;

   function Pad (Str : String; Length : Natural) return String is
      Retval : String (1 .. Length) := (others => ' ');
   begin
      if Str'Length > Length then
         Retval := Str (Str'First .. Str'First + Length - 1);
      else
         Retval (1 .. Str'Length) := Str;
      end if;
      return Retval;
   end Pad;

   procedure Add_Trivia (City : String; Information : String) is
      Key    : constant City_Name_T := Pad (City, City_Name_T'Length);
      Info   : constant Information_T :=
        Pad (Information, Information_T'Length);
      Cursor : Maps.Cursor;
      List   : Information_List.Vector;
   begin
      Cursor := Map.Find (Key);
      if Cursor = Maps.No_Element then
         List.Append (Info);
         Map.Insert (Key => Key, New_Item => List);
      else
         List := Maps.Element (Cursor);
         List.Append (Info);
         Map.Replace_Element (Position => Cursor, New_Item => List);
      end if;
   end Add_Trivia;
   --Database_Body_1

   --Database_Body_2
   function Get_Trivia (City : String) return Information_List.Vector is
      Ret_Val : Information_List.Vector;
      Key     : constant City_Name_T := Pad (City, City_Name_T'Length);
      Cursor  : Maps.Cursor;
   begin
      Cursor := Map.Find (Key);
      if Cursor /= Maps.No_Element then
         Ret_Val := Maps.Element (Cursor);
      end if;
      Information_Sort.Sort (Ret_Val);
      return Ret_Val;
   end Get_Trivia;

   function Get_Cities return City_List.Vector is
      Ret_Val   : City_List.Vector;
      Cursor    : Maps.Cursor := Map.First;
      To_Append : City_Name_T;
   begin
      while Cursor /= Maps.No_Element loop
         To_Append := Maps.Key (Cursor);
         Ret_Val.Append (Pad (To_Append, City_Name_T'Length));
         exit when Cursor = Map.Last;
         Cursor := Maps.Next (Cursor);
      end loop;
      City_Sort.Sort (Ret_Val);
      return Ret_Val;
   end Get_Cities;
   --Database_Body_2
end City_Trivia;
