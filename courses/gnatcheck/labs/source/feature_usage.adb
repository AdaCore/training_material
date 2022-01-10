package body Feature_Usage is

   procedure Complex_Inlined (Flag : in out Integer) is
   begin
      if Flag > 100 then
         declare
            C_Inverse : constant Integer := 0 - Flag;
         begin
            Flag := C_Inverse;
         end;
      end if;
   end Complex_Inlined;

end Feature_Usage;
