--
--            Copyright (C) 2008-2010, AdaCore
--

package body Search_Leads is

   ----------
   -- Save --
   ----------

   procedure Save (Into : in out Repository;
                   Pos  : Maze.Position;
                   Path : Traversal.Trail)
   is
   begin
      Insert (Into, Search_Info'(Pos, Path));
   end Save;

   -------------
   -- Restore --
   -------------

   procedure Restore (From : in out Repository;
                      Pos  :    out Maze.Position;
                      Path :    out Traversal.Trail)
   is
      Temp : Search_Info;
   begin
      Remove (From, Temp);
      Pos := Temp.Pos;
      Path := Temp.Path;
   end Restore;

   -----------
   -- Empty --
   -----------

   function Empty (This : Repository) return Boolean is
   begin
      return Size (This) = 0;
   end Empty;

end Search_Leads;
