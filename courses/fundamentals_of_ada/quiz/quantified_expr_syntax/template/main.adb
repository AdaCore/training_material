-- Which declaration(s) is(are) legal?

procedure Main is
   --$ begin cut
   function F (S : String) return Boolean is
     (for all C of S => C /= ' ');
   --$ end cut
   --$ begin cut
   function F (S : String) return Boolean is
     (not for some C of S => C = ' ');
   -- Parentheses required around the quantified expression
   --$ end cut
   --$ begin cut
   function F (S : String) return String is
     (for all C of S => C);
   -- Must return a :ada:`Boolean`
   --$ end cut
   --$ begin cut
   function F (S : String) return String is
     (if (for all C of S => C /= ' ') then "OK"
      else "NOK");
   --$ end cut
begin
   null;
end Main;
