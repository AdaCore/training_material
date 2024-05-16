with Ada.Text_IO; use Ada.Text_IO;
package body Queens_Pkg is

   procedure Generate (Count : Positive) is
      Board : array (1 .. Count, 1 .. Count) of Boolean :=
        (others =>
           (others => False));
      function Test
        (Row, Column : Integer)
         return Boolean is
      begin
         for J in 1 .. Column - 1
         loop
            if
              (Board (Row, J)
               or else (Row > J and then Board (Row - J, Column - J))
               or else (Row + J <= Count and then Board (Row + J, Column - J)))
            then
               return False;
            end if;
         end loop;
         return True;
      end Test;
      function Fill
        (Column : Integer)
         return Boolean is
      begin
         for Row in Board'Range (1)
         loop
            if Test (Row, Column)
            then
               Board (Row, Column) := True;
               if Column = Count or else Fill (Column + 1)
               then
                  return True;
               end if;
               Board (Row, Column) := False;
            end if;
         end loop;
         return False;
      end Fill;
   begin
      if not Fill (1)
      then
         raise Program_Error;
      end if;
      for I in Board'Range (1)
      loop
         Put (Integer'Image (Count + 1 - I));
         for J in Board'Range (2)
         loop
            if Board (I, J)
            then
               Put ("|Q");
            elsif (I + J) mod 2 = 1
            then
               Put ("|/");
            else
               Put ("| ");
            end if;
         end loop;
         Put_Line ("|");
      end loop;
      declare
         Col : Character := 'A';
      begin
         Put ("   ");
         for I in 1 .. Count
         loop
            Put (Col & " ");
            Col := Character'succ (Col);
         end loop;
         New_Line;
      end;
   end Generate;

end Queens_Pkg;
