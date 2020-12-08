with System; use System;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;
with SDL_pixels_h; use SDL_pixels_h;

package body Display.Basic.Utils is

      ---------------
   -- Put_Pixel --
   ---------------
   package Address_To_Pixel is new
     System.Address_To_Access_Conversions (SDL_stdinc_h.Uint32);

   function RGBA_To_Uint32(Screen : access SDL_Surface;
                           Color : RGBA_T) return Uint32 is
   begin
     return SDL_MapRGBA (Screen.format,
                   unsigned_char (Color.R),
                   unsigned_char (Color.G),
                   unsigned_char (Color.B),
                   unsigned_char (Color.A));

   end RGBA_To_Uint32;


   procedure Put_Pixel_Slow (Screen : access SDL_Surface;
                        X, Y : Integer; Color : RGBA_T)
   is
   begin
      --  Just ignore out of screen draws
      if
        X >= Integer(Screen.w) or else X < 0
        or else Y >= Integer(Screen.h) or else Y < 0
      then
         return;
      end if;

      declare

         use System.Storage_Elements;
         Offset : Storage_Offset :=
           Storage_Offset ((Y * Integer (Screen.w) + X)
                           * Integer (Screen.format.all.BytesPerPixel));
         Pixels : System.Address := Screen.pixels + Offset;
      begin
         Address_To_Pixel.To_Pointer (Pixels).all :=
           SDL_MapRGBA (Screen.format,
                        unsigned_char (Color.R),
                        unsigned_char (Color.G),
                        unsigned_char (Color.B),
                        unsigned_char (Color.A));
      end;
   end Put_Pixel_Slow;

   procedure Put_Pixel (Screen : access SDL_Surface;
                        X, Y : Integer; Color : Uint32)
   is
   begin
      --  Just ignore out of screen draws
      if
        X >= Integer(Screen.w) or else X < 0
        or else Y >= Integer(Screen.h) or else Y < 0
      then
         return;
      end if;

      declare

         use System.Storage_Elements;
         Offset : Storage_Offset :=
           Storage_Offset ((Y * Integer (Screen.w) + X)
                           * Integer (Screen.format.all.BytesPerPixel));
         Pixels : System.Address := Screen.pixels + Offset;
      begin
         Address_To_Pixel.To_Pointer (Pixels).all := Color;
      end;
   end Put_Pixel;




   Nb_Canvas : Integer := 0;

   function Register_SDL_Surface(S : access SDL_Surface) return Canvas_ID is
      Current_Id : Canvas_ID;
   begin
      if Nb_Canvas = Internal_Canvas'Length then
         raise Too_Many_Canvas;
      end if;

      Current_Id := Canvas_ID(Integer(Internal_Canvas'First) + Nb_Canvas);
      Internal_Canvas(Current_Id) := T_Internal_Canvas'(Surface     => S,
                                                        Zoom_Factor => 1.0,
                                                        Center => (0, 0));
      Nb_Canvas := Nb_Canvas + 1;
      return Current_Id;
   end Register_SDL_Surface;



   function Get_Internal_Canvas(Canvas : Canvas_ID) return T_Internal_Canvas is
   begin
      return Internal_Canvas (Canvas);
   end Get_Internal_Canvas;

   procedure Set_Center (Canvas : Canvas_ID; Center : Screen_Point) is
   begin

      Internal_Canvas(Canvas).Center := Center;
   end Set_Center;

   function Get_Center (Canvas : Canvas_ID) return Screen_Point is
   begin
      return Internal_Canvas(Canvas).Center;
   end Get_Center;

   procedure Set_Zoom_Factor (Canvas : Canvas_ID; ZF : Float) is
   begin

      Internal_Canvas(Canvas).Zoom_Factor := ZF;
   end Set_Zoom_Factor;



end Display.Basic.Utils;
