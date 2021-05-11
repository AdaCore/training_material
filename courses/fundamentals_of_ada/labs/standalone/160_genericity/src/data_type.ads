package Data_Type is

  type Record_T is tagged record
    Field1 : Integer;
    Field2 : Character;
  end record;

  function "<"
   (L, R : Record_T)
    return Boolean is
   (if L.Field2 < R.Field2 then True elsif L.Field2 > R.Field2 then False
    else L.Field1 < R.Field1);

  function Image
   (Element : Record_T)
    return String is
   (Element.Field2 & " =>" & Integer'Image (Element.Field1));

end Data_Type;
