with Types;
package List is

   procedure Add
     (Left     : Types.Numeric_T;
      Operator : Character;
      Right    : Types.Numeric_T);

   function Length return Natural;

   function Element
     (Index : Integer)
      return Types.Record_T;

end List;
