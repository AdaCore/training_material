-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package Global_Options is
   pragma Elaborate_Body;  --  so that the parsing is done before any accesses

   Width, Height : Positive;
   --  The width and height of a new maze to be created.

   Perfect : Boolean := False;
   --  Whether a newly generated maze is to have only one solution; so-called
   --  "perfect" mazes have only one. Ordinarily we will have more than one
   --  because the point of the primary program is to find all the possible
   --  unique solutions to a given maze in the shortest time possible.

   File_Name : Unbounded_String;
   --  The name of a file containing a previously-generated maze.

   Display_Output : Boolean := True;
   --  Whether to display solutions and the maze itself. Ordinarily output is
   --  appropriate, but for the sake of timing studies it is convenient to
   --  disable output.

   Max_Searchers : Positive := 4;
   --  The maximum number of searcher threads provided by the pool. The value
   --  itself is arbitrary, however it can certainly make a difference in that
   --  having more physical processors than searchers is sub-optimal.

   --  These flags are used internally to check for conflicting switches, as
   --  well as to check switch completeness. For example, they are used to
   --  check that the width and height are not specified if the filename is
   --  specified. Similarly, they are used to verify that if one of the width
   --  or height switch is specified that the other switch is also specified.
   --
   --  Applications read them too so they are made visible.

   Filename_Set : Boolean := False;
   --  Was the "-f" switch specified?

   Width_Set : Boolean := False;
   --  Was the "-w" switch specified?

   Height_Set : Boolean := False;
   --  Was the "-h" switch specified?

   Arguments_Invalid : Boolean := False;
   --  set by internal command line arg parser

end Global_Options;
