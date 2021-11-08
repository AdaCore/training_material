with Unbounded_String_Pkg; use Unbounded_String_Pkg;

procedure Test is
   U1 : Ustring_T;
begin
   U1 := To_Ustring_T ("Hello");
   declare
      U2 : Ustring_T;
   begin
      U2 := To_Ustring_T ("Goodbye");
      U1 := U2; -- Reclaims U1 memory
   end; -- Reclaims U2 memory
end Test; -- Reclaims U1 memory
