package body Operations is

   function Perform (Operation : Operation_T) return Numeric_T is
   begin
      if abs (Operation.Left) > Maximum_Value
        or else abs (Operation.Right) > Maximum_Value
      then
         raise Out_Of_Range;
      else
         case Operation.Operator is
            when '/' =>
               if Operation.Right = 0.0 then
                  raise Divide_By_Zero;
               else
                  return Numeric_T (Operation.Left / Operation.Right);
               end if;

            when '*' =>
               return Numeric_T (Operation.Left * Operation.Right);

            when '-' =>
               return Numeric_T (Operation.Left - Operation.Right);

            when '+' =>
               return Numeric_T (Operation.Left + Operation.Right);
         end case;
      end if;
   end Perform;

end Operations;
