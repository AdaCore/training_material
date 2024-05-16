with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Exceptions;
with Console;
with Inventory;

package body Loading_Dock is

   package Asf renames Ada.Strings.Fixed;

   procedure Load_From_Manifest (Manifest : String) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Manifest);
      while not Ada.Text_IO.End_Of_File (File)
      loop
         declare
            Line      : constant String := Ada.Text_IO.Get_Line (File);
            Separator : Integer         := Asf.Index (Line, "|");
         begin
            if Separator not in Line'Range
            then
               Console.Print ("Illegal entry: " & Line);
            else
               declare
                  Item : constant String :=
                    Asf.Trim
                      (Line (Line'First .. Separator - 1), Ada.Strings.Both);
                  Count : constant String :=
                    Asf.Trim
                      (Line (Separator + 1 .. Line'Last), Ada.Strings.Both);
               begin
                  if Item'Length > 0 and then Count'Length > 0
                  then
                     Inventory.Add (Item, Positive'Value (Count));
                  end if;
               exception
                  when The_Err : others =>
                     Console.Print
                       ("Invalid item (" & Item & ") or count (" & Count &
                        ")");
               end;
            end if;
         end;
      end loop;
   exception
      when The_Err : others =>
         Console.Print
           ("Error reading '" & Manifest & "': " &
            Ada.Exceptions.Exception_Message (The_Err));
   end Load_From_Manifest;

end Loading_Dock;
