--
--            Copyright (C) 2008-2010, AdaCore
--

with Console;         use Console;
with Global_Options;  use Global_Options;

package body Traversal.Display is

   Total : Natural := 0;
   --  The total number of solutions displayed. We declare it here, rather than
   --  the private part of the package, because it is not incremented when the
   --  Display_Output switch is set to false and would thus be useless to child
   --  units.

   ----------
   -- Show --
   ----------

   procedure Show (Solution : Trail;  On : Maze.Reference) is
   begin
      if Display_Output then
         declare
            Display       : Console.Device renames Console.Instance.all;
            Safe_Location : constant Location := Safe_Position (Display, On);
         begin
            Traversal.Plot (Solution);
            Total := Total + 1;
            Display.Put (Total'Img & " solution(s) ", Safe_Location);
         end;
      end if;
   end Show;

end Traversal.Display;
