package body Loggers is

   X : Integer := 0;

   ------------------------
   -- Severity_To_String --
   ------------------------

   function Severity_To_String
     (Level : Severity_Level)
      return String is
   begin
      X := X + 1;
      X := X + 2;
      X := X + 3;
      X := X + 4;
      X := X + 5;
      X := X + 6;
      X := X + 7;
      X := X + 8;
      X := X + 9;
      X := X + 10;
      X := X + 11;
      return Level'Image;
   end Severity_To_String;

   -----------------
   -- Log_Message --
   -----------------

   overriding procedure Log_Message
     (Logger  : in out Console_Logger;
      Level   :        String;
      Message :        String) is
   begin
      X := X + 1;
      X := X + 2;
      X := X + 3;
      X := X + 4;
      X := X + 5;
      X := X + 6;
      X := X + 7;
      X := X + 8;
      X := X + 9;
      X := X + 10;
      X := X + 11;
      X := X + 12;
      X := X + 13;
      X := X + 14;
      X := X + 15;
      X := X + 16;
      X := X + 17;
      X := X + 18;
      X := X + 19;
      X := X + 20;
      X := X + 21;
      X := X + 22;
      X := X + 23;
      X := X + 24;
      X := X + 25;
      X := X + 26;
      X := X + 27;
      X := X + 28;
      X := X + 29;
      X := X + 30;
      X := X + 31;
      X := X + 32;
      X := X + 33;
      X := X + 34;
      X := X + 35;
      X := X + 36;
      X := X + 37;
      X := X + 38;
      X := X + 39;
      X := X + 40;
      X := X + 41;
      X := X + 42;
      X := X + 43;
      X := X + 44;
      X := X + 45;
      X := X + 46;
      X := X + 47;
      X := X + 48;
      X := X + 49;
      X := X + 50;
      X := X + 51;
   end Log_Message;

   -------------------------
   -- Enable_Color_Output --
   -------------------------

   procedure Enable_Color_Output
     (Logger : in out Console_Logger;
      Enable :        Boolean) is
   begin
      --  Comment Line 1
      --  Comment Line 2
      --  Comment Line 3
      --  Comment Line 4
      --  Comment Line 5
      --  Comment Line 6
      --  Comment Line 7
      --  Comment Line 8
      --  Comment Line 9
      --  Comment Line 10
      --  Comment Line 11
      --  Comment Line 12
      --  Comment Line 13
      --  Comment Line 14
      --  Comment Line 15
      --  Comment Line 16
      --  Comment Line 17
      --  Comment Line 18
      --  Comment Line 19
      --  Comment Line 20
      --  Comment Line 21
      --  Comment Line 22
      --  Comment Line 23
      --  Comment Line 24
      --  Comment Line 25
      --  Comment Line 26
      --  Comment Line 27
      --  Comment Line 28
      --  Comment Line 29
      --  Comment Line 30
      --  Comment Line 31
      null;
   end Enable_Color_Output;

   ----------------
   -- Should_Log --
   ----------------

   overriding function Should_Log
     (Filter : Minimum_Severity_Filter;
      Level  : String)
      return Boolean is
   begin
      X := X + 1;
      X := X + 2;
      X := X + 3;
      X := X + 4;
      X := X + 5;
      X := X + 6;
      X := X + 7;
      X := X + 8;
      X := X + 9;
      X := X + 10;
      X := X + 11;
      return True;
   end Should_Log;

   -------------------
   -- Create_Filter --
   -------------------

   function Create_Filter
     (Min_Level : Severity_Level)
      return Minimum_Severity_Filter is
   begin
      X := X + 1;
      X := X + 2;
      X := X + 3;
      X := X + 4;
      X := X + 5;
      X := X + 6;
      X := X + 7;
      X := X + 8;
      X := X + 9;
      X := X + 10;
      X := X + 11;
      X := X + 12;
      X := X + 13;
      X := X + 14;
      X := X + 15;
      X := X + 16;
      X := X + 17;
      X := X + 18;
      X := X + 19;
      X := X + 20;
      X := X + 21;
      X := X + 22;
      X := X + 23;
      X := X + 24;
      X := X + 25;
      X := X + 26;
      X := X + 27;
      X := X + 28;
      X := X + 29;
      X := X + 30;
      X := X + 31;
      return (Min_Level => Min_Level);
   end Create_Filter;

end Loggers;
