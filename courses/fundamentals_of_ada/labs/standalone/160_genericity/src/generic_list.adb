package body Generic_List is

  procedure Add
   (This : in out List_T;
    Item : in     Element_T) is
  begin
    This.Length               := This.Length + 1;
    This.Values (This.Length) := Item;
  end Add;

  procedure Sort (This : in out List_T) is
    Temp : Element_T;
  begin
    for I in 1 .. This.Length loop
      for J in I + 1 .. This.Length loop
        if This.Values (J) < This.Values (J - 1) then
          Temp                := This.Values (J);
          This.Values (J)     := This.Values (J - 1);
          This.Values (J - 1) := Temp;
        end if;
      end loop;
    end loop;
  end Sort;

end Generic_List;
