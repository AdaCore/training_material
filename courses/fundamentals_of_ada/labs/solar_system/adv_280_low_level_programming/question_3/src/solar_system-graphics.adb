with Ada.Real_Time; use Ada.Real_Time;

package body Solar_System.Graphics is

   procedure Draw_Body(Object : Body_Type; Canvas : Canvas_ID) is
   begin
      if Object.Visible then
         Draw_Sphere(Canvas   => Canvas,
                     Position => (Object.Pos.X, Object.Pos.Y, 0.0),
                     Radius   => Object.Radius,
                     Color    => Object.Color);
         if Object.With_Tail then
            for I in T_Tail'Range loop
               Draw_Sphere(Canvas   => Canvas,
                           Position => (Object.Tail(I).X, Object.Tail(I).Y, 0.0),
                           Radius   => Object.Radius,
                           Color    => Object.Color);
            end loop;
         end if;
      end if;
   end Draw_Body;

   protected body Graphic_Context is
      procedure Set_Window(W : Window_ID) is
      begin
         Window := W;
         Canvas := Get_Canvas(W);
         Is_Set := True;
      end Set_Window;

      entry Get_Window(W : out Window_ID; C : out Canvas_ID) when Is_Set is
      begin
         W := Window;
         C := Canvas;
      end Get_Window;
   end Graphic_Context;

   task body T_Display is
      --  declare a variable Now of type Time to record current time
      Now : Time;
      --  declare a constant Period of 40 milliseconds of type Time_Span defining the loop period
      Period  : constant Time_Span := Milliseconds (30);
      Canvas : Canvas_ID;
      Window : Window_ID;
   begin
      Graphic_Context.Get_Window(Window, Canvas);
      loop
         Now := Clock;
         for B of Bodies loop
            Draw_Body(Object => B.Get_Data,
                      Canvas => Canvas);
         end loop;

         Swap_Buffers(Window);

         delay until Now + Period;
      end loop;

   end T_Display;


end Solar_System.Graphics;
