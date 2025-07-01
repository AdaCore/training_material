--Database_List_Substance

--Main
with Simple_Io; use Simple_Io;
with Database;
with Database_List;
procedure Main is
   List      : Database_List.List_T;
   Component : Database.Database_T;

   procedure Add is
      Value : constant String := Get_String ("Add");
   begin
      if Value'Length > 0 then
         Component := Database.To_Database (Value);
         Database_List.Insert (List, Component);
      end if;
   end Add;

   procedure Delete is
      Value : constant String := Get_String ("Delete");
   begin
      if Value'Length > 0 then
         Component := Database.To_Database (Value);
         Database_List.Delete (List, Component);
      end if;
   end Delete;

   procedure Print is
   begin
      Database_List.First (List);
      Simple_Io.Print_String ("List");
      while not Database_List.End_Of_List (List) loop
         Component := Database_List.Current (List);
         Print_String ("  " & Database.From_Database (Component));
         Database_List.Next (List);
      end loop;
   end Print;

begin
   loop
      case Get_Character ("A=Add D=Delete P=Print Q=Quit") is
         when 'a' | 'A' =>
            Add;
         when 'd' | 'D' =>
            Delete;
         when 'p' | 'P' =>
            Print;
         when 'q' | 'Q' =>
            exit;
         when others =>
            null;
      end case;
   end loop;
end Main;
