package Solar_System.Graphics is

   protected Graphic_Context is
      procedure Set_Window (W : Window_ID);
      entry Get_Window (W : out Window_ID; C : out Canvas_ID);
   private
      Window : Window_ID;
      Canvas : Canvas_ID;
      Is_Set : Boolean := False;
   end Graphic_Context;

   task T_Display;

private

   procedure Draw_Body (Object : Body_Type; Canvas : Canvas_ID);

end Solar_System.Graphics;
