with Ada.Containers.Bounded_Ordered_Maps;
package body City_Trivia is

  use type Strings_Vector_T;
  package Maps is new Ada.Containers.Bounded_Ordered_Maps
   (Key_Type => Unbounded_String, Element_Type => Strings_Vector_T);
  use type Maps.Cursor;
  Map : Maps.Map (100);

  procedure Add_Trivia
   (City        : String;
    Information : String) is
    Key    : Unbounded_String := To_Unbounded_String (City);
    Info   : Unbounded_String := To_Unbounded_String (Information);
    Cursor : Maps.Cursor;
  begin
    Cursor := Map.Find (Key);
    if Cursor = Maps.No_Element then
      declare
        To_Insert : Strings_Vector_T;
      begin
        To_Insert.Append (Info);
        Map.Insert
         (Key      => Key,
          New_Item => To_Insert);
      end;
    else
      declare
        Current : Strings_Vector_T;
      begin
        Current := Maps.Element (Cursor);
        Current.Append (Info);
        Map.Replace_Element
         (Position => Cursor,
          New_Item => Current);
      end;
    end if;
  end Add_Trivia;

  function Get_Trivia
   (City : String)
    return Strings_Vector_T is
    Ret_Val : Strings_Vector_T;
    Key     : Unbounded_String := To_Unbounded_String (City);
    Cursor  : Maps.Cursor;
  begin
    Cursor := Map.Find (Key);
    if Cursor /= Maps.No_Element then
      Ret_Val := Maps.Element (Cursor);
    end if;
    Sort.Sort (Ret_Val);
    return Ret_Val;
  end Get_Trivia;

  function Get_Keys return Strings_Vector_T is
    Ret_Val : Strings_Vector_T;
    Cursor  : Maps.Cursor := Map.First;
  begin
    while Cursor /= Maps.No_Element loop
      declare
        To_Append : Unbounded_String := Maps.Key (Cursor);
      begin
        Ret_Val.Append (To_Append);
      end;
      exit when Cursor = Map.Last;
      Cursor := Maps.Next (Cursor);
    end loop;
    return Ret_Val;
  end Get_Keys;

end City_Trivia;
