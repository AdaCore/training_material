package body Ops is
   procedure Apply
     (Op :        Op_Kind;
      X  : in out Integer) is
   begin
      case Op is
         when Increment =>
            X := X + 1;
         when Decrement =>
            X := X - 1;
         when Double =>
            X := X * 2;
         when Half =>
            X := X / 2;
      end case;
   exception
      when others =>
         null;
   end Apply;
end Ops;
