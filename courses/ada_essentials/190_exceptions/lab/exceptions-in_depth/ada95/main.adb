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
     (1 =>
        ((1.1, '+', 2.2),
         Success    => False,
         Result     => 0.0,
         E_Xception => Null_Id),
      2 =>
        ((33.3, '-', 44.4),
         Success    => False,
         Result     => 0.0,
         E_Xception => Null_Id),
      3 =>
        ((555.5, '*', 666.6),
         Success    => False,
         Result     => 0.0,
         E_Xception => Null_Id),
      4 =>
        ((77.7, '/', 88.8),
         Success    => False,
         Result     => 0.0,
         E_Xception => Null_Id),
      5 =>
        ((9.9, '/', 0.0),
         Success    => False,
         Result     => 0.0,
         E_Xception => Null_Id),
      6 =>
        ((9.9, '*', 8.8),
         Success    => False,
         Result     => 0.0,
         E_Xception => Null_Id),
      7 =>
        ((7.7, '/', 0.06),
         Success    => False,
         Result     => 0.0,
         E_Xception => Null_Id));

   procedure Perform (Expression : in out Operation_T) is
   begin
      Expression.Result := Operations.Perform (Expression.Operation);
      Expression.Success := True;
   exception
      when The_Err : others =>
         Expression.E_Xception := Exception_Identity (The_Err);
   end Perform;

begin

   for Idx in Expressions'Range loop
      Perform (Expressions (Idx));
   end loop;

   for Idx in Expressions'Range loop
      Put
        (Float'Image (Expressions (Idx).Operation.Left)
         & " "
         & Operations.Operator_T'Image (Expressions (Idx).Operation.Operator)
         & " "
         & Float'Image (Expressions (Idx).Operation.Right));
      if Expressions (Idx).Success then
         Put_Line
           (" = " & Operations.Numeric_T'Image (Expressions (Idx).Result));
      else
         Put_Line (" raises " & Exception_Name (Expressions (Idx).E_Xception));
      end if;
   end loop;

end Main;
