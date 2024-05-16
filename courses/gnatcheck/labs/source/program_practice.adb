package body Program_Practice is

   function Test
     (Flag1 : in Boolean;
      Flag2 : in Boolean)
      return Boolean is
   begin
      return Flag1 or Flag2;
   end Test;

   function Test
     (Flag : Character)
      return Character is
   begin
      case Flag is
         when 'A' =>
            return 'a';
         when 'a' =>
            return 'A';
         when others =>
            return Character'succ (Flag);
      end case;
   exception
      when Constraint_Error =>
         return Character'First;
      when others =>
         return Character'Last;
   end Test;

end Program_Practice;
