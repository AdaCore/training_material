with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with Operations;

procedure Main is

   type Operation_T is record
      Operation  : Operations.Operation_T;
      Success    : Boolean := False;
      Result     : Operations.Numeric_T := 0.0;
      E_Xception : Exception_Id := Null_Id;
   end record;

   Expressions : array (1 .. 7) of Operation_T :=
     (1 => ((1.1, '+', 2.2), others => <>),
      2 => ((33.3, '-', 44.4), others => <>),
      3 => ((555.5, '*', 666.6), others => <>),
      4 => ((77.7, '/', 88.8), others => <>),
      5 => ((9.9, '/', 0.0), others => <>),
      6 => ((9.9, '*', 8.8), others => <>),
      7 => ((7.7, '/', 0.06), others => <>));

   procedure Perform (Expression : in out Operation_T) is
   begin
      Expression.Result := Operations.Perform (Expression.Operation);
      Expression.Success := True;
   exception
      when The_Err : others =>
         Expression.E_Xception := Exception_Identity (The_Err);
   end Perform;

begin

   for Expression of Expressions loop
      Perform (Expression);
   end loop;

   for Expression of Expressions loop
      Put
        (Expression.Operation.Left'Image
         & " "
         & Expression.Operation.Operator'Image
         & " "
         & Expression.Operation.Right'Image);
      if Expression.Success then
         Put_Line (" = " & Expression.Result'Image);
      else
         Put_Line (" raises " & Exception_Name (Expression.E_Xception));
      end if;
   end loop;

end Main;
