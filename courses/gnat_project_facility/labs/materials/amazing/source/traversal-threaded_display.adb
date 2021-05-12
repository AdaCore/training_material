-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------

with Console;        use Console;
with Global_Options; use Global_Options;

package body Traversal.Threaded_Display is

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
            Mutex.Acquire;
            Total := Total + 1;
            Traversal.Plot (Solution);
            Display.Put (Total'Img & " solution(s) ", Safe_Location);
            Mutex.Release;
         end;
      end if;
   end Show;

   -----------
   -- Mutex --
   -----------

   protected body Mutex is

      entry Acquire when Count > 0 is
      begin
         Count := Count - 1;
      end Acquire;

      procedure Release is
      begin
         Count := Count + 1;
      end Release;

   end Mutex;

end Traversal.Threaded_Display;
