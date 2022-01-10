--Database_List_Substance

--Main
with Simple_Io; use Simple_Io;
with Database;
with Database_List;
procedure Main is
   List    : Database_List.List_T;
   Element : Database.Database_T;

   procedure Add is
      Value : constant String := Get_String ("Add");
   begin
      if Value'length > 0 then
         Element := Database.To_Database (Value);
         Database_List.Insert (List, Element);
      end if;
   end Add;

   procedure Delete is
      Value : constant String := Get_String ("Delete");
   begin
      if Value'length > 0 then
         Element := Database.To_Database (Value);
         Database_List.Delete (List, Element);
      end if;
   end Delete;

   procedure Print is
   begin
      Database_List.First (List);
      Simple_Io.Print_String ("List");
      while not Database_List.End_Of_List (List) loop
         Element := Database_List.Current (List);
         Print_String ("  " & Database.From_Database (Element));
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
