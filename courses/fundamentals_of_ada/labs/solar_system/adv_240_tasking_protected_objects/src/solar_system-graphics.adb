package body Solar_System.Graphics is

   procedure Draw_Body (Object : Body_T; Canvas : Canvas_ID) is
   begin
      if Object.Visible then
         Draw_Sphere
           (Canvas   => Canvas,
            Position => (Object.X, Object.Y, 0.0),
            Radius   => Object.Radius,
            Color    => Object.Color);
      end if;
   end Draw_Body;

   procedure Draw_All (Bodies : Bodies_Array_T; Canvas : Canvas_ID) is
   begin
      for Obj of Bodies loop
         if Obj.Visible then
            Draw_Body (Obj, Canvas);
         end if;
      end loop;

   end Draw_All;

end Solar_System.Graphics;
