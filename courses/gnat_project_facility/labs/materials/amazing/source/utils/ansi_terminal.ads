--
--            Copyright (C) 2008-2010, AdaCore
--

--  This package provides ANSI (American National Standards Institute) "escape
--  sequences" to be used in implementing a console device on operating systems
--  that support such sequences. Most Linux and Unix versions will support ANSI
--  sequences.
--
--  This package is a generic so that both buffered and unbuffered I/O can be
--  supported. Note that the packages Ada.Text_IO and GNAT.IO both export
--  procedures that match the generic formal subprogram parameter.

generic
   with procedure Put (Output : String) is <>;
package ANSI_Terminal is

   subtype Rows    is Positive;
   subtype Columns is Positive;

   type Position is
      record
         Row  : Rows    := Rows'First;
         Col  : Columns := Columns'First;
      end record;

   type Offset is
      record
         Row : Natural;
         Col : Natural;
      end record;

   type Switch is (Off, On);

   function "+" (Left : Position;  Right : Offset) return Position;
   function "-" (Left : Position;  Right : Offset) return Position;

   procedure Move_Cursor (To : Position);

   procedure Home;

   procedure Clear_Screen;

   procedure Cursor_Up (Count : Rows := 1);

   procedure Cursor_Down (Count : Rows := 1);

   procedure Cursor_Forward (Count : Columns := 1);

   procedure Cursor_Backward (Count : Columns := 1);

   procedure Cursor (Control : Switch);

   procedure Attributes_Off;

   function Move_Cursor (To : Position) return String;

   function Home return String;

   function Clear_Screen return String;

   function Cursor_Up (Count : Rows := 1) return String;

   function Cursor_Down (Count : Rows := 1) return String;

   function Cursor_Forward (Count : Columns := 1) return String;

   function Cursor_Backward (Count : Columns := 1) return String;

   function Cursor (Control : Switch) return String;

   function Attributes_Off return String;

   function Reverse_Video return String;

   function Blink return String;

   function Underscore return String;

   function Bold return String;

end ANSI_Terminal;
