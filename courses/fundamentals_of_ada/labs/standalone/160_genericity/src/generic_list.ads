generic
  type Element_T is private;
  Max_Size : Natural;
  with function "<"
   (L, R : Element_T)
    return Boolean is <>;
package Generic_List is

  type List_T is tagged private;

  procedure Add
   (This : in out List_T;
    Item : in     Element_T);

  procedure Sort (This : in out List_T);

private

  subtype Index_T is Natural range 0 .. Max_Size;
  type List_Array_T is array (1 .. Index_T'Last) of Element_T;

  type List_T is tagged record
    Values : List_Array_T;
    Length : Index_T := 0;
  end record;

end Generic_List;
