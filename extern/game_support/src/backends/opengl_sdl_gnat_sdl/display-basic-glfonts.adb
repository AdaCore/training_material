with SDL_SDL_video_h; use SDL_SDL_video_h;
with SDL_SDL_stdinc_h; use SDL_SDL_stdinc_h;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO;
with Display.Basic.Fonts; use Display.Basic.Fonts;

package body Display.Basic.GLFonts is

--           function SDL_DEFINE_PIXELFORMAT(stype : Uint32;
--                                     order : Uint32;
--                                     layout : Uint32;
--                                     bits: Uint32;
--                                     bytes: Uint32) return Uint32 is (2 ** 28
--                                                                      or
--                                                                        (stype * 2 ** 24)
--                                                                      or
--                                                                        (order * 2**20)
--                                                                      or
--                                                                        (layout * 2 ** 16)
--                                                                      or
--                                                                        (bits * 2 ** 8)
--                                                                      or
--                                                                        bytes
--                                                                     );
--
--        SDL_PIXELFORMAT_ABGR8888 : Uint32 := SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_TYPE'Pos(SDL_PIXELTYPE_PACKED32),
--                                                                    SDL_PACKEDORDER_TYPE'Pos(SDL_PACKEDORDER_ABGR),
--                                                                    SDL_PACKEDLAYOUT_TYPE'Pos(SDL_PACKEDLAYOUT_8888),
--                                                                    32, 4);
--
--
--        SDL_PIXELFORMAT_RGBA32 : Uint32 := SDL_PIXELFORMAT_ABGR8888;


   subtype Visible_Char_Idx is Character range ' ' .. '~';
   type Char_Texture_Array_T is array (Visible_Char_Idx) of GLUint;
   Char_Texture_Ids : Char_Texture_Array_T;
   Is_Char_Texture_Initialized : boolean := False;

   procedure InitCharTextures is
      S : access SDL_Surface;
    --  Format : UInt32 := SDL_PIXELFORMAT_RGBA32;
      Texture_ID : aliased GLuint;
   begin
      for C in Visible_Char_Idx loop
         glGenTextures(1, Texture_ID'Access);
         Char_Texture_Ids(C) := Texture_ID;
         glBindTexture(GL_TEXTURE_2D, Texture_ID);

         --S := SDL_CreateRGBSurfaceWithFormat(0, 8, 8, 32, Format);

         S := SDL_CreateRGBSurface(0, 8, 8, 32, 16#FF#, 16#FF00#,16#FF0000#,16#FF000000#);
         if S = null then
            Ada.Text_IO.Put_Line ("Cant create surface");
         else
            Draw_Char(S,
                      Screen_Point'(0, 0),
                      C,
                      Font8x8,
                      SDL_MapRGBA(S.format.all'Address, 255, 255, 255, 255),
                      SDL_MapRGBA(S.format.all'Address, 0, 0, 0, 0));
            glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA, 8, 8, 0,  GL_RGBA, GL_UNSIGNED_BYTE, S.pixels);
            if glGetError /= GL_NO_ERROR then
               Ada.Text_IO.Put_Line ("Error in texture");
            end if;
            SDL_FreeSurface (S);

            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
         end if;
      end loop;
      Is_Char_Texture_Initialized := true;
   end InitCharTextures;

   function getCharTexture(Char : Character) return GLUint is
   begin
      if not Is_Char_Texture_Initialized then
         InitCharTextures;
      end if;
      return Char_Texture_Ids(Char);
   end getCharTexture;


end Display.Basic.GLFonts;
