with Ada.Text_IO; use Ada.Text_IO;
package body Generic_List.Output is

  procedure Print (List : List_T) is
  begin
    for I in 1 .. List.Length loop
      Put_Line (Integer'Image (I) & ") " & Image (List.Values (I)));
    end loop;
  end Print;

end Generic_List.Output;
