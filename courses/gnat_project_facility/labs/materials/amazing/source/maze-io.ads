--
--            Copyright (C) 2008-2010, AdaCore
--

package Maze.IO is

   procedure Save_Maze (File_Name : String);
   --  Writes the singleton maze representation to the file named File_Name.
   --  Raises Ada.IO_Exceptions.Name_Error as necessary.

   procedure Restore_Maze (File_Name : String);
   --  Recreates the singleton maze representation from the file named
   --  File_Name.
   --  Raises Ada.IO_Exceptions.Name_Error as necessary.

end Maze.IO;
