
with Ada.Text_IO;
with Messages;
with Messages.List_Types;
with Messages.List_Types.Operations;

procedure Main is
   package Types renames Messages.List_Types;
   package Operations renames Messages.List_Types.Operations;

   List : Types.List_T;
   Head : Types.List_T;

   function Convert
     (Value : Integer)
      return Messages.Message_T is
      Ret_Value : Messages.Message_T;
   begin
      Messages.Set_Content (Ret_Value, Value);
      return Ret_Value;
   end Convert;

   procedure Add_One (Value : Integer) is
   begin
      Operations.Append (List, Convert (Value));
      List := Operations.Next (List);
   end Add_One;

begin
   Operations.Append (List, Convert (1));
   Head := List;
   Add_One (23);
   Add_One (456);
   Add_One (78);
   Add_One (9);
   Ada.Text_IO.Put_Line (Operations.Image (Head));
end Main;
