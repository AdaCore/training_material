--
--            Copyright (C) 2008-2010, AdaCore
--

package body ANSI_Terminal is

   Attributes_Off_Command : constant String := ASCII.Esc & "[0m";
   Home_Command           : constant String := ASCII.Esc & "[H";
   Clear_Screen_Command   : constant String := ASCII.Esc & "[2J";
   Enable_Cursor_Command  : constant String := ASCII.ESC & "[?25h";
   Disable_Cursor_Command : constant String := ASCII.ESC & "[?25l";
   Enable_Bold            : constant String := ASCII.Esc & "[1m";
   Enable_Underscore      : constant String := ASCII.Esc & "[4m";
   Enable_Blink           : constant String := ASCII.Esc & "[5m";
   Enable_Reverse_Video   : constant String := ASCII.Esc & "[7m";

   Prefix                 : constant String := ASCII.Esc & '[';

   Move_Cursor_Command    : constant String := Prefix & "00;00H";

   ---------------
   -- As_String --
   ---------------

   function As_String (Input : Integer) return String is
      Image           : constant String := Integer'Image (Input);
      Unsigned_Buffer : String (1 .. 2);
   begin
      Unsigned_Buffer := "00";
      if Input > 9 then -- note can only be 2 digits
         Unsigned_Buffer := Image (2 .. 3);
      else -- single digit " x"
         Unsigned_Buffer (2) := Image (2);
      end if;
      return Unsigned_Buffer;
   end As_String;

   --------------------
   -- Attributes_Off --
   --------------------

   procedure Attributes_Off is
   begin
      Put (Attributes_Off_Command);
   end Attributes_Off;

   -----------------
   -- Move_Cursor --
   -----------------

   procedure Move_Cursor (To : Position) is
      Move_Cursor_Buffer : String (1 .. 8) := Move_Cursor_Command;
      subtype Row_Slice is Integer range 3 .. 4;
      subtype Col_Slice is Integer range 6 .. 7;
   begin
      Move_Cursor_Buffer (Row_Slice) := As_String (To.Row);
      Move_Cursor_Buffer (Col_Slice) := As_String (To.Col);
      Put (Move_Cursor_Buffer);
   end Move_Cursor;

   ----------
   -- Home --
   ----------

   procedure Home is
   begin
      Put (Home_Command);
   end Home;

   ------------------
   -- Clear_Screen --
   ------------------

   procedure Clear_Screen is
   begin
      Put (Clear_Screen_Command);
   end Clear_Screen;

   --------------------
   -- Cursor_Forward --
   --------------------

   procedure Cursor_Forward (Count : Columns := 1) is
   begin
      Put (Prefix & As_String (Count) & 'C');
   end Cursor_Forward;

   ---------------------
   -- Cursor_Backward --
   ---------------------

   procedure Cursor_Backward (Count : Columns := 1) is
   begin
      Put (Prefix & As_String (Count) & 'D');
   end Cursor_Backward;

   -----------------
   -- Cursor_Down --
   -----------------

   procedure Cursor_Down (Count : Rows := 1) is
   begin
      Put (Prefix & As_String (Count) & 'B');
   end Cursor_Down;

   ---------------
   -- Cursor_Up --
   ---------------

   procedure Cursor_Up (Count : Rows := 1) is
   begin
      Put (Prefix & As_String (Count) & 'A');
   end Cursor_Up;

   ------------
   -- Cursor --
   ------------

   procedure Cursor (Control : Switch) is
   begin
      if Control = On then
         Put (Enable_Cursor_Command);
      else
         Put (Disable_Cursor_Command);
      end if;
   end Cursor;

   --------------------
   -- Attributes_Off --
   --------------------

   function Attributes_Off return String is
   begin
      return Attributes_Off_Command;
   end Attributes_Off;

   -----------------
   -- Move_Cursor --
   -----------------

   function Move_Cursor (To : Position) return String is
   begin
      return Prefix & As_String (To.Row) & ";" & As_String (To.Col) & "H";
   end Move_Cursor;

   ----------
   -- Home --
   ----------

   function Home return String is
   begin
      return Home_Command;
   end Home;

   ------------------
   -- Clear_Screen --
   ------------------

   function Clear_Screen return String is
   begin
      return Clear_Screen_Command;
   end Clear_Screen;

   ---------------
   -- Cursor_Up --
   ---------------

   function Cursor_Up (Count : Rows := 1) return String is
   begin
      return Prefix & As_String (Count) & 'A';
   end Cursor_Up;

   -----------------
   -- Cursor_Down --
   -----------------

   function Cursor_Down (Count : Rows := 1) return String is
   begin
      return Prefix & As_String (Count) & 'B';
   end Cursor_Down;

   --------------------
   -- Cursor_Forward --
   --------------------

   function Cursor_Forward (Count : Columns := 1) return String is
   begin
      return Prefix & As_String (Count) & 'C';
   end Cursor_Forward;

   ---------------------
   -- Cursor_Backward --
   ---------------------

   function Cursor_Backward (Count : Columns := 1) return String is
   begin
      return Prefix & As_String (Count) & 'D';
   end Cursor_Backward;

   ------------
   -- Cursor --
   ------------

   function Cursor (Control : Switch) return String is
   begin
      if Control = On then
         return Enable_Cursor_Command;
      else
         return Disable_Cursor_Command;
      end if;
   end Cursor;

   ----------
   -- Bold --
   ----------

   function Bold return String is
   begin
      return Enable_Bold;
   end Bold;

   ----------------
   -- Underscore --
   ----------------

   function Underscore return String is
   begin
      return Enable_Underscore;
   end Underscore;

   -----------
   -- Blink --
   -----------

   function Blink return String is
   begin
      return Enable_Blink;
   end Blink;

   -------------------
   -- Reverse_Video --
   -------------------

   function Reverse_Video return String is
   begin
      return Enable_Reverse_Video;
   end Reverse_Video;

   ---------
   -- "+" --
   ---------

   function "+" (Left : Position;  Right : Offset) return Position is
   begin
      return Position'(Left.Row + Right.Row, Left.Col + Right.Col);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Position;  Right : Offset) return Position is
   begin
      return Position'(Left.Row - Right.Row, Left.Col - Right.Col);
   end "-";

end ANSI_Terminal;
