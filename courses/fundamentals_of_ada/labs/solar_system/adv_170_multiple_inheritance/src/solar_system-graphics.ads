package Solar_System.Graphics is
   
   procedure Draw_All (Bodies : Bodies_Array_T; Canvas : Canvas_ID);
   
private
   
   procedure Draw_Body(Object : Body_T; Canvas : Canvas_ID);
   
end Solar_System.Graphics;
