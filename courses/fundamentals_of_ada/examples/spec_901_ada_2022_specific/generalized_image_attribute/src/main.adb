with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type R is record
      I : Integer;
   end record;

   type A is array (1 .. 4) of R;

   task type T (V : Integer) is
   end T;

   task body T is
      Obj : Integer;
   begin
      null;
   end T;

   Record_Obj : R := (I => 1);
   Array_Obj : aliased A := [Record_Obj, Record_Obj, Record_Obj, Record_Obj];
   Acc_O : access A := Array_Obj'Access;
   Task_Obj : T (2);

begin

   --$ begin cut
   Put_Line (Record_Obj'Image);
   Put_Line (Array_Obj'Image);
   Put_Line (Acc_O'Image);
   Put_Line (Task_Obj'Image);
   --$ end cut

end Main;
