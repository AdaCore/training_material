with Data_Type;
with Generic_List;
procedure Main is
   package List is new Generic_List (Element_T => Data_Type.Record_T);
   --  Need to add formal parameters as necessary

   My_List : List.List_T;
   Element : Data_Type.Record_T;

begin
   --  Add some items to the list
   List.Add (My_List, Element);
   List.Add (My_List, Element);
   List.Add (My_List, Element);

   List.Sort (My_List);
   List.Print (My_List);
end Main;
