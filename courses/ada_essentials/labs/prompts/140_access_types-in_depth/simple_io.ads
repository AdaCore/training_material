with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Simple_Io is
   function Get_String
     (Prompt : String)
      return String;
   function Get_Number
     (Prompt : String)
      return Integer;
   function Get_Character
     (Prompt : String)
      return Character;
   procedure Print_String (Str : String);
   procedure Print_Number (Num : Integer);
   procedure Print_Character (Char : Character);
   function Get_String
     (Prompt : String)
      return Unbounded_String;
   procedure Print_String (Str : Unbounded_String);
end Simple_Io;
