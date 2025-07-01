   procedure My_Put_Image
     (Output : in out
        Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Obj    : R)
   is
   begin
      Output.Put ("my very own null record");
   end My_Put_Image;
