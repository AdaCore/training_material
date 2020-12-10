with Screen_Interface; use Screen_Interface;

package Fonts is

   type BMP_Font is (Font8x8, Font12x12, Font16x24);

   procedure Draw_Char
     (X, Y   : Integer;
      Char   : Character;
      Font   : BMP_Font;
      FG, BG : Color);

   procedure Draw_Char
     (X, Y   : Integer;
      Char   : Character;
      Font   : BMP_Font;
      FG     : Color);

   procedure Draw_String
     (X, Y   : Integer;
      Str    : String;
      Font   : BMP_Font;
      FG, BG : Color;
      Wrap   : Boolean := False);

   procedure Draw_String
     (X, Y   : Integer;
      Str    : String;
      Font   : BMP_Font;
      FG     : Color;
      Wrap   : Boolean := False);

   function Char_Size (Font : BMP_Font) return Point;

   function String_Size (Font : BMP_Font; Text : String) return Point;

end Fonts;
