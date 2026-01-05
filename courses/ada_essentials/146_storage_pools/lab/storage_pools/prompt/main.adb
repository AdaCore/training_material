with Database;

procedure Main is

begin
   Database.Add (12);
   Database.Add (34.5);
   Database.Add ('D');
   Database.Remove (34.5);
   Database.Add (45.6);
   Database.Print;
end Main;
