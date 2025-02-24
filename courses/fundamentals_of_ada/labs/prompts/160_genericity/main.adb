with Data_Type;
with Generic_List;
procedure Main is
   package List is new Generic_List (Component_T => Data_Type.Record_T);
   --  Need to add formal parameters as necessary

   My_List : List.List_T;
   Component : Data_Type.Record_T;

begin
   --  Add some items to the list
   List.Add (My_List, Component);
   List.Add (My_List, Component);
   List.Add (My_List, Component);

   List.Sort (My_List);
   List.Print (My_List);
end Main;
