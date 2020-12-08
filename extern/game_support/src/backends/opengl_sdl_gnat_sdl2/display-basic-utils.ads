with SDL_video_h; use SDL_video_h;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with SDL_stdinc_h; use SDL_stdinc_h;
with SDL_surface_h; use SDL_surface_h;

package Display.Basic.Utils is
   

   function RGBA_To_Uint32(Screen : access SDL_Surface;
                           Color : RGBA_T) return Uint32;
   
   procedure Put_Pixel_Slow (Screen : access SDL_Surface;
                        X, Y : Integer; Color : RGBA_T);
   
   procedure Put_Pixel (Screen : access SDL_Surface;
                        X, Y : Integer; Color : Uint32);

   
   type T_Internal_Canvas is record
      Surface : access SDL_Surface;
      Zoom_Factor : Float := 1.0;
      Center : Screen_Point := (0, 0);
   end record;
   
   procedure Set_Zoom_Factor (Canvas : Canvas_Id; ZF : Float);
   procedure Set_Center (Canvas : Canvas_ID; Center : Screen_Point);
   function Get_Center (Canvas : Canvas_ID) return Screen_Point;

   
   function Register_SDL_Surface(S : access SDL_Surface) return Canvas_ID;
   procedure Update_SDL_Surface(Canvas : Canvas_ID; S : access SDL_Surface);
   function Get_Internal_Canvas(Canvas : Canvas_ID) return T_Internal_Canvas with Inline;
  
   
private
   type Internal_Canvas_Array is array (Canvas_ID) of T_Internal_Canvas;

   Internal_Canvas : Internal_Canvas_Array;
   

end Display.Basic.Utils;
