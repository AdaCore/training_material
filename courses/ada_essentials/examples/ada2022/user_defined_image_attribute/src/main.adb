with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Text_Buffers;

procedure Main is

   --$ begin cut
   type R is null record with
     Put_Image => My_Put_Image;
   --$ end cut

   -- Providing the spec is mandatory due to freeze-point rules and
   -- the early use of it in the Put_Image aspect
   procedure My_Put_Image
     (Output : in out
        Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Obj    : R);

   --$ begin cut
   procedure My_Put_Image
     (Output : in out
        Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Obj    : R)
   is
   begin
      Output.Put ("my very own null record");
   end My_Put_Image;
   --$ end cut

   O : R;

begin
   Put_Line (O'Image);
end Main;
