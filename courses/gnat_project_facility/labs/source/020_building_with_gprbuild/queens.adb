with Ada.Command_Line;
with Queens_Pkg;
procedure Queens is
   Count : Positive := 8;
begin
   if Ada.Command_Line.Argument_Count > 0
   then
      begin
         Count := Positive'value (Ada.Command_Line.Argument (1));
      exception
         when others =>
            Count := 9;
      end;
   end if;
   Queens_Pkg.Generate (Count);
end Queens;
