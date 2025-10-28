package body Validator is

   function Is_Valid
     (Object : Types.Record_T)
      return Boolean is
      Result : Float;
   begin
      case Object.Operator is
         when '+' =>
            Result := Float (Object.Left) + Float (Object.Right);
         when '-' =>
            Result := Float (Object.Left) - Float (Object.Right);
         when '*' =>
            Result := Float (Object.Left) * Float (Object.Right);
         when '/' =>
            Result := Float (Object.Left) / Float (Object.Right);
         when others =>
            --  If the operator isn't legal, make sure
            --  the result is out of range
            Result := Float (Types.Maximum_Value) * 2.0;
      end case;
      return
        Result in Float (Types.Minimum_Value) .. Float (Types.Maximum_Value);
   end Is_Valid;

end Validator;
