package Loggers is

   type Abstract_Logger is abstract tagged limited private;
   type Severity_Level is (Info, Warning, Error, Critical);

   procedure Log_Message
     (Logger  : in out Abstract_Logger;
      Level   :        String;
      Message :        String) is abstract;

   type Abstract_Formatted_Logger is abstract new Abstract_Logger with private;

   procedure Format_Header
     (Logger    :     Abstract_Formatted_Logger;
      Category  :     String;
      Formatted : out String) is abstract;

   type Abstract_Buffered_Logger is abstract new Abstract_Logger with private;

   procedure Flush (Logger : in out Abstract_Buffered_Logger) is abstract;

   type Abstract_Log_Filter is abstract tagged private;

   function Should_Log
     (Filter : Abstract_Log_Filter;
      Level  : String)
      return Boolean is abstract;

   function Severity_To_String
     (Level : Severity_Level)
      return String;

   type Console_Logger is new Abstract_Logger with private;

   overriding procedure Log_Message
     (Logger  : in out Console_Logger;
      Level   :        String;
      Message :        String);

   procedure Enable_Color_Output
     (Logger : in out Console_Logger;
      Enable :        Boolean);

   type Minimum_Severity_Filter is new Abstract_Log_Filter with private;

   overriding function Should_Log
     (Filter : Minimum_Severity_Filter;
      Level  : String)
      return Boolean;

   function Create_Filter
     (Min_Level : Severity_Level)
      return Minimum_Severity_Filter;

private

   type Abstract_Logger is abstract tagged limited null record;
   type Abstract_Formatted_Logger is
      abstract new Abstract_Logger with null record;
   type Abstract_Buffered_Logger is
      abstract new Abstract_Logger with null record;
   type Abstract_Log_Filter is abstract tagged null record;

   type Console_Logger is new Abstract_Logger with record
      Use_Color : Boolean := False;
   end record;

   type Minimum_Severity_Filter is new Abstract_Log_Filter with record
      Min_Level : Severity_Level := Info;
   end record;

end Loggers;
