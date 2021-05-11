
with Ada.Text_IO; use Ada.Text_IO;
with Types;       use Types;
with Types.Strings;

procedure Main is

   Aft : Integer renames Types.Strings.Digits_After_Decimal;
   Exp : Integer renames Types.Strings.Exponent_Digits;

   Feet       : Feet_T;
   Miles      : Miles_T;
   Kilometers : Kilometers_T;

   Seconds : Seconds_T;
   Minutes : Minutes_T;
   Hours   : Hours_T;

   Mph : Mph_T;

   function Get
     (Prompt : String)
      return String is
   begin
      Put (Prompt & "> ");
      return Get_Line;
   end Get;

begin
   Feet       := Feet_T'Value (Get ("Feet"));
   Miles      := Miles_T'Value (Get ("Miles"));
   Kilometers := Kilometers_T'Value (Get ("Kilometers"));

   Seconds := Seconds_T'Value (Get ("Seconds"));
   Minutes := Minutes_T'Value (Get ("Minutes"));
   Hours   := Hours_T'Value (Get ("Hours"));

   Aft := 2;
   Exp := 2;
   Mph := Feet / Seconds;
   Put_Line
     (Strings.To_String (Feet) & " feet / " & Strings.To_String (Seconds) &
      " seconds = " & Strings.To_String (Mph) & " mph");
   Aft := Aft + 1;
   Exp := Exp + 1;
   Mph := Miles / Hours;
   Put_Line
     (Strings.To_String (Miles) & " miles / " & Strings.To_String (Hours) &
      " hour = " & Strings.To_String (Mph) & " mph");
   Aft := Aft + 1;
   Exp := Exp + 1;
   Mph := Kilometers / Minutes;
   Put_Line
     (Strings.To_String (Kilometers) & " km / " & Strings.To_String (Minutes) &
      " minute = " & Strings.To_String (Mph) & " mph");

end Main;
