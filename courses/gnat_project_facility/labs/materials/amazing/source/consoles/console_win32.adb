-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------

pragma C_Pass_By_Copy (128);

with GNAT.IO;
with Interfaces;
with Ada.Exceptions;  use Ada.Exceptions;

package body Console is

   procedure Move_Cursor (To : Console.Location);
   --  An internal, non-primitive routine that works directly at the Win32
   --  level.

   type Colors is (Black, Blue, Green, Cyan, Red, Magenta, Brown, Gray,
                   Light_Blue, Light_Green, Light_Cyan, Light_Red,
                   Light_Magenta, Yellow, White);

   function Current_Foreground return Colors;
   function Current_Background return Colors;

   procedure Set_Foreground (Color : Colors);
   procedure Set_Background (Color : Colors);

   procedure Plot_Point
     (This       : in out Device;
      Here       : Console.Location;
      Foreground : Colors;
      Background : Colors);

   --------------
   -- Instance --
   --------------

   function Instance return Reference is
   begin
      return The_Instance;
   end Instance;

   ---------
   -- Put --
   ---------

   procedure Put
      (This  : in out Device;
       Char  : Character;
       Point : Console.Location)
   is
      pragma Unreferenced (This);
   begin
      Move_Cursor (Point);
      GNAT.IO.Put (Char);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
      (This  : in out Device;
       Str   : String;
       Point : Console.Location)
   is
      pragma Unreferenced (This);
   begin
      Move_Cursor (Point);
      GNAT.IO.Put (Str);
   end Put;

   ----------------
   -- Plot_Point --
   ----------------

   procedure Plot_Point
      (This       : in out Device;
       Here       : Console.Location;
       Foreground : Colors;
       Background : Colors)
   is
      pragma Unreferenced (This);
      Previous_Foreground : constant Colors := Current_Foreground;
      Previous_Background : constant Colors := Current_Background;
      Target              : Console.Location;
      Rectangle_Fill      : constant String := " # ";
   begin
      Set_Foreground (Foreground);
      Set_Background (Background);
      Target.Row := Here.Row * 2;
      --  the center of the cell at each physical row (ie the one displayed) is
      --  twice the logical row number because we use multiple characters to
      --  depict individual cells
      Target.Column := (Here.Column * 4) - 2;
      --  likewise, the physical column displayed is 4 times the logical value
      Move_Cursor (Target);
      GNAT.IO.Put (Rectangle_Fill);
      Set_Foreground (Previous_Foreground);
      Set_Background (Previous_Background);
   exception
      when others =>
         Set_Foreground (Previous_Foreground);
         Set_Background (Previous_Background);
         raise;
   end Plot_Point;

   -------------------------
   -- Plot_Solution_Point --
   -------------------------

   procedure Plot_Solution_Point
     (This : in out Device;  Here : Console.Location)
   is
   begin
      Plot_Point (This, Here, Foreground => Magenta, Background => Black);
   end Plot_Solution_Point;

   -----------------------------
   -- Plot_Intersection_Point --
   -----------------------------

   procedure Plot_Intersection_Point
     (This  : in out Device;
      Here  : Console.Location)
   is
   begin
      Plot_Point (This, Here, Foreground => Cyan, Background => Black);
   end Plot_Intersection_Point;

   --  these are the strings used to depict individual maze cells, which is
   --  why some contain spaces
   Horizontal_Wall       : constant String := "+---"; --  both North and South
   Empty_Horizontal_Wall : constant String := "+   ";
   West_Wall             : constant String := "|   ";
   Empty_West_Wall       : constant String := "    ";
   East_Wall             : constant String := "|";
   Corner                : constant String := "+";

   ----------
   -- Draw --
   ----------

   procedure Draw (This : in out Device;  The_Maze : access Maze.Puzzle) is
      R_Offset            : Natural;
      C_Offset            : Natural;
      Previous_Foreground : constant Colors := Current_Foreground;
      Previous_Background : constant Colors := Current_Background;
      Current             : Maze.Position;
      use Maze, Console;
   begin
      Set_Foreground (Light_Green);
      Set_Background (Black);

      --  The row and column offsets translate between the logical row/column
      --  values and the physical screen coordinates of the characters used to
      --  represent the cells. This is necessary because each cell is depicted
      --  by several characters both horizontally and vertically, such that the
      --  screen coordinates for a given maze cell are not simply at their
      --  corresponding maze row/column pair.

      R_Offset := 0;

      for R in 1 .. Maze.Rows (The_Maze) loop

         C_Offset := 0;

         for C in 1 .. Maze.Columns (The_Maze) loop

            Current := Make_Position (R, C);

            if North_Wall_Present (The_Maze, Current) then
               Put (This, Horizontal_Wall,
                    Location'(R + R_Offset, C + C_Offset));
            else
               Put (This, Empty_Horizontal_Wall,
                    Location'(R + R_Offset, C + C_Offset));
            end if;
            if West_Wall_Present (The_Maze, Current) then
               Put (This, West_Wall,
                    Location'(R + R_Offset + 1, C + C_Offset));
            else
               Put (This, Empty_West_Wall,
                    Location'(R + R_Offset + 1, C + C_Offset));
            end if;

            if R = Maze.Rows (The_Maze) then
               --  last row, so print the bottom border
               if South_Wall_Present (The_Maze, Current) then
                  Put (This, Horizontal_Wall,
                       Location'(R + R_Offset + 2, C + C_Offset));
               else
                  Put (This, Empty_Horizontal_Wall,
                       Location'(R + R_Offset + 2, C + C_Offset));
               end if;
               if C = Maze.Columns (The_Maze) then -- last row and column
                  --  print the bottom-right corner
                  Put (This, Corner,
                       Location'(R + R_Offset + 2, C + C_Offset + 4));
               end if;
            end if;

            C_Offset := C_Offset + 3;
            --  so that we are now on the east edge

            --  print the right-most corners on the east border and each east
            --  wall that is present
            if C = Maze.Columns (The_Maze) then -- last column
               Put (This, Corner, Location'(R + R_Offset, C + C_Offset + 1));
               if East_Wall_Present (The_Maze, Current) then
                  Put (This, East_Wall,
                       Location'(R + R_Offset + 1, C + C_Offset + 1));
               end if;
            end if;

         end loop; -- columns

         R_Offset := R_Offset + 1;
      end loop; --  rows

      Set_Foreground (Previous_Foreground);
      Set_Background (Previous_Background);
   exception
      when others =>
         Set_Foreground (Previous_Foreground);
         Set_Background (Previous_Background);
         raise;
   end Draw;

   -------------------
   -- Safe_Position --
   -------------------

   function Safe_Position (This : Device;  The_Maze : access Maze.Puzzle)
      return Console.Location
   is
      pragma Unreferenced (This);
   begin
      return Console.Location'((Maze.Rows (The_Maze) * 2) + 4, 1);
   end Safe_Position;

   Win32_Subsystem_Error : exception;

   use Interfaces;

   subtype SHORT  is Integer_16;
   subtype DWORD  is Unsigned_32;
   subtype HANDLE is Unsigned_32;

   type LPDWORD is access all DWORD;
   for LPDWORD'Storage_Size use 0;

   pragma Convention (C, LPDWORD);

   type Unsigned_4 is mod 2 ** 4;

   type Attribute is
      record
         Foreground : Unsigned_4;
         Background : Unsigned_4;
         Reserved   : Unsigned_8 := 0;
      end record;

   for Attribute use
      record
         Foreground at 0 range 0 .. 3;
         Background at 0 range 4 .. 7;
         Reserved   at 1 range 0 .. 7;
      end record;

   for Attribute'Alignment use 1;

   pragma Convention (C, Attribute);

   type COORD is
      record
         X : SHORT;
         Y : SHORT;
      end record;

   pragma Convention (C, COORD);

   type SMALL_RECT is
      record
         Left   : SHORT;
         Top    : SHORT;
         Right  : SHORT;
         Bottom : SHORT;
      end record;

   pragma Convention (C, SMALL_RECT);

   type CONSOLE_SCREEN_BUFFER_INFO is
      record
         Size       : COORD;
         Cursor_Pos : COORD;
         Attrib     : Attribute;
         Window     : SMALL_RECT;
         Max_Size   : COORD;
      end record;

   pragma Convention (C, CONSOLE_SCREEN_BUFFER_INFO);

   type PCONSOLE_SCREEN_BUFFER_INFO is access all CONSOLE_SCREEN_BUFFER_INFO;
   for PCONSOLE_SCREEN_BUFFER_INFO'Storage_Size use 0;

   pragma Convention (C, PCONSOLE_SCREEN_BUFFER_INFO);

   WIN32_ERROR          : constant DWORD  := 0;
   INVALID_HANDLE_VALUE : constant HANDLE := -1;
   STD_OUTPUT_DEVICE    : constant DWORD  := -11;

   As_Number : constant array (Colors) of Unsigned_4 :=
      (0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15);
   --  note the missing value 8

   As_Color  : constant array (Unsigned_4) of Colors :=
      (Black, Blue, Green, Cyan, Red, Magenta, Brown, Gray,
       Black, Light_Blue, Light_Green, Light_Cyan, Light_Red,
       Light_Magenta, Yellow, White);

   Std_Output : HANDLE;
   --  we can safely share the standard output device handle

   function GetStdHandle (Value : DWORD) return HANDLE;
   pragma Import (StdCall, GetStdHandle, "GetStdHandle");

   function SetConsoleCursorPosition
      (Buffer : HANDLE;
       Pos    : COORD)
       return DWORD;
   pragma Import (StdCall, SetConsoleCursorPosition,
                  "SetConsoleCursorPosition");

   function SetConsoleTextAttribute
      (Buffer : HANDLE;
       Attr   : Attribute)
       return DWORD;
   pragma Import (StdCall, SetConsoleTextAttribute,
                  "SetConsoleTextAttribute");

   function GetConsoleScreenBufferInfo
      (Buffer : HANDLE;
       Info   : PCONSOLE_SCREEN_BUFFER_INFO)
       return DWORD;
   pragma Import (StdCall, GetConsoleScreenBufferInfo,
                  "GetConsoleScreenBufferInfo");

   function FillConsoleOutputCharacter
      (Console : HANDLE;
       Char    : Character;
       Length  : DWORD;
       Start   : COORD;
       Written : LPDWORD)
       return DWORD;
   pragma Import (Stdcall, FillConsoleOutputCharacter,
                  "FillConsoleOutputCharacterA");

   function FillConsoleOutputAttribute
      (Console : Handle;
       Attr    : Attribute;
       Length  : DWORD;
       Start   : COORD;
       Written : LPDWORD)
       return DWORD;
   pragma Import (Stdcall, FillConsoleOutputAttribute,
                  "FillConsoleOutputAttribute");

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Device) is
      pragma Unreferenced (This);
      Attr          : Attribute;
      Origin        : constant COORD := (0, 0);
      Length : constant DWORD := DWORD ((SHORT'Last + 1) * (SHORT'Last + 1));
      --  ie, maximum possible screen buffer width * max height
      Num_Bytes     : aliased DWORD;
      Num_Bytes_Ref : constant LPDWORD := Num_Bytes'Unchecked_Access;
      Screen_Buffer : aliased CONSOLE_SCREEN_BUFFER_INFO;
      Buffer_Info   : constant PCONSOLE_SCREEN_BUFFER_INFO :=
        Screen_Buffer'Unchecked_Access;
   begin
      --  read the window's screen buffer settings
      if GetConsoleScreenBufferInfo
        (Std_Output, Buffer_Info) = WIN32_ERROR
      then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "error getting screen buffer settings in Clear");
      end if;
      --  set the attributes for any characters written after this call
      Attr.Background := As_Number (Black);
      Attr.Foreground := Screen_Buffer.Attrib.Foreground;
      if SetConsoleTextAttribute (Std_Output, Attr) = WIN32_ERROR then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "error setting character attributes in Clear");
      end if;
      --  Set the character attributes for all the characters in the buffer,
      --  beginning at the origin. Num_Bytes is how many were actually written,
      --  but we ignore that and just check for the overall error. We are
      --  specifying more characters to set than are present in the buffer, but
      --  the routine only writes up to the end of the screen buffer so no
      --  overwrite is occurring.  The characters themselves are not changed.
      if FillConsoleOutputAttribute
         (Std_Output, Attr, Length, Origin, Num_Bytes_Ref) = WIN32_ERROR
      then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "error applying character attributes in Clear");
      end if;
      --  Write a blank character to the console screen buffer to every
      --  possible screen buffer location (since Length is the max width * max
      --  height) beginning at the origin. Stops writing at the end of the
      --  screen buffer so there is no overrun problem. We ignore how many were
      --  actually written.
      if FillConsoleOutputCharacter
         (Std_Output, ' ', Length, Origin, Num_Bytes_Ref) = WIN32_ERROR
      then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "error writing filler characters in Clear");
      end if;
   end Clear;

   ------------------------
   -- Current_Foreground --
   ------------------------

   function Current_Foreground return Colors is
      Screen_Buffer : aliased CONSOLE_SCREEN_BUFFER_INFO;
      Buffer_Info   : constant PCONSOLE_SCREEN_BUFFER_INFO :=
        Screen_Buffer'Unchecked_Access;
   begin
      if GetConsoleScreenBufferInfo
        (Std_Output, Buffer_Info) = WIN32_ERROR
      then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "GetConsoleScreenBufferInfo error");
      end if;
      return As_Color (Screen_Buffer.Attrib.Foreground);
   end Current_Foreground;

   ------------------------
   -- Current_Background --
   ------------------------

   function Current_Background return Colors is
      Screen_Buffer : aliased CONSOLE_SCREEN_BUFFER_INFO;
      Buffer_Info   : constant PCONSOLE_SCREEN_BUFFER_INFO :=
        Screen_Buffer'Unchecked_Access;
   begin
      if GetConsoleScreenBufferInfo
        (Std_Output, Buffer_Info) = WIN32_ERROR
      then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "GetConsoleScreenBufferInfo error");
      end if;
      return As_Color (Screen_Buffer.Attrib.Background);
   end Current_Background;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground (Color : Colors) is
      Attr          : Attribute;
      Screen_Buffer : aliased CONSOLE_SCREEN_BUFFER_INFO;
      Buffer_Info   : constant PCONSOLE_SCREEN_BUFFER_INFO :=
        Screen_Buffer'Unchecked_Access;
   begin
      if GetConsoleScreenBufferInfo
        (Std_Output, Buffer_Info) = WIN32_ERROR
      then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "GetConsoleScreenBufferInfo error");
      end if;
      Attr.Foreground := As_Number (Color);
      Attr.Background := Screen_Buffer.Attrib.Background;
      --  all characters subsequently written will now have this foreground
      --  color and the existing background color
      if SetConsoleTextAttribute (Std_Output, Attr) = WIN32_ERROR then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "Set attribute error in Set_Foreground");
      end if;
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Color : Colors) is
      Attr          : Attribute;
      Screen_Buffer : aliased CONSOLE_SCREEN_BUFFER_INFO;
      Buffer_Info   : constant PCONSOLE_SCREEN_BUFFER_INFO :=
        Screen_Buffer'Unchecked_Access;
   begin
      if GetConsoleScreenBufferInfo
        (Std_Output, Buffer_Info) = WIN32_ERROR
      then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "GetConsoleScreenBufferInfo error");
      end if;
      Attr.Foreground := Screen_Buffer.Attrib.Foreground;
      Attr.Background := As_Number (Color);
      --  all characters subsequently written will now have this background
      --  color and the existing foreground color
      if SetConsoleTextAttribute (Std_Output, Attr) = WIN32_ERROR then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "Set attribute error in Set_Background");
      end if;
   end Set_Background;

   -----------------
   -- Move_Cursor --
   -----------------

   procedure Move_Cursor (To : Console.Location) is
      Target        : COORD := (SHORT (To.Column) - 1, SHORT (To.Row) - 1);
      --  note that we must reverse the order of rows and columns when going
      --  from console.location to windows X and Y, and also subtract 1 since
      --  windows coordinates start at zero
      Screen_Buffer : aliased CONSOLE_SCREEN_BUFFER_INFO;
      Buffer_Info   : constant PCONSOLE_SCREEN_BUFFER_INFO :=
        Screen_Buffer'Unchecked_Access;
   begin
      if GetConsoleScreenBufferInfo
        (Std_Output, Buffer_Info) = WIN32_ERROR
      then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "GetConsoleScreenBufferInfo error");
      end if;
      --  bracket the requested position to the dimensions of the screen buffer
      Target.X := SHORT'Min (Target.X, Screen_Buffer.Size.X);
      Target.Y := SHORT'Min (Target.Y, Screen_Buffer.Size.Y);
      --  move the console cursor to Target
      if SetConsoleCursorPosition (Std_Output, Target) = WIN32_ERROR then
         Raise_Exception (Win32_Subsystem_Error'Identity,
                          "Cursor position error attempting (X,Y) (" &
                          Target.X'Img & ", " & Target.Y'Img & ')');
      end if;
   end Move_Cursor;

begin
   --  get a handle to the standard output device for the window
   Std_Output := GetStdHandle (STD_OUTPUT_DEVICE);
   if Std_Output = INVALID_HANDLE_VALUE then
      Raise_Exception (Win32_Subsystem_Error'Identity,
                       "Invalid standard output handle obtained");
   end if;
end Console;
