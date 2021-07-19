--
--            Copyright (C) 2008-2010, AdaCore
--

package HCI is  --  human-computer interaction

   procedure Show_Usage;
   --  Displays text explaining the required arguments.

   procedure Optionally_Save_Maze (Unsaved : Boolean);
   --  If Unsaved is True, asks if the user wants to save the maze. If so, asks
   --  for the name of the file and then stores it in a stream file. We don't
   --  need a parameter for the maze because it is a singleton, globally
   --  available via the Maze package.

end HCI;
