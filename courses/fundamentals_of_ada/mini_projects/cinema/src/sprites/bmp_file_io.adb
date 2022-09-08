with Interfaces; use Interfaces;
with Surfaces; use Surfaces;
with Pixels;

package body BMP_File_IO is
   -- Bitmap file format definition:
   -- The file is composed of
   --
   --   +------------------------+
   --   |         Header         |
   --   +------------------------+
   --   |          Info          |
   --   +------------------------+
   --   | Compression (optional) |
   --   +------------------------+
   --   |   Palette (optional)   |
   --   +------------------------+
   --   |         Pixels         |
   --   +------------------------+
   --
   -- Compression Header is present iif Info.Compression is /= 0 and /= 3
   -- (this is the case for the files given)
   --
   -- Palette is present iif Palette_Size /= 0
   --
   -- Palette colors are stored as 4 bytes entries
   --
   --  +---+---+---+------------+
   --  | R | G | B | Reservered |
   --  +---+---+---+------------+
   --
   -- If there is no palette, pixels are directly stored, as either
   -- 4 bytes (Pixel_Size = 32).
   -- This is the case for the dummy files, and for the rotating_triangle movie.
   --
   --  +---+---+---+------------+
   --  | R | G | B | Reservered |
   --  +---+---+---+------------+
   --
   -- or 3 bytes (Pixel_Size = 24). This is the case for the sunset file.
   --
   --  +---+---+---+
   --  | R | G | B |
   --  +---+---+---+
   --
   -- Else, if there is a palette, Pixels are stored as an Index to the palette,
   -- of size Info.Pixel_Size. This is the case for the nian cat movie.
   --
   --  +-----------+
   --  | Color_Idx |
   --  +-----------+

   type U8_Array is array (Natural range <>) of Unsigned_8;
   type U16_Array is array (Natural range <>) of Unsigned_16;

   type Header (As_Array : Boolean := True) is record
      case As_Array is
         when True =>
            Arr : U8_Array (1 .. 14);
         when False =>
            Signature : Integer_16;
            Size      : Integer_32; --  File size
            Reserved1 : Integer_16;
            Reserved2 : Integer_16;
            Offset    : Integer_32; --  Data offset
      end case;
   end record with Unchecked_Union, Pack, Size => 14 * 8;

   type Info (As_Array : Boolean := True) is record
      case As_Array is
         when True =>
            Arr : U8_Array (1 .. 40);
         when False =>
            Struct_Size   : Integer_32;
            Width         : Integer_32; -- Image width in pixels
            Height        : Integer_32; -- Image hieght in pixels
            Planes        : Integer_16;
            Pixel_Size    : Integer_16; -- Bits per pixel
            Compression   : Integer_32; -- Zero means no compression
            Image_Size    : Integer_32; -- Size of the image data in UInt8s
            PPMX          : Integer_32; -- Pixels per meter in x led
            PPMY          : Integer_32; -- Pixels per meter in y led
            Palette_Size  : Integer_32; -- Number of colors
            Important     : Integer_32;
      end case;
   end record with Unchecked_Union, Pack, Size => 40 * 8;
   
   subtype Pix_Val_Arr_32 is U8_Array (1 .. 4);
   -- Only supporting 256 x 32 bits palettes
   subtype Color_Table_Size_T is Natural range Natural'First .. 256 * Pix_Val_Arr_32'Length;

   type Color_Array is array (Positive range <>) of Pix_Val_Arr_32;
   type Color_Table (Size : Color_Table_Size_T := Color_Table_Size_T'First) is record
      Arr : Color_Array (1 .. Size);
   end record with Pack;

   procedure Read_Surface_Data
     (Surf : in out Surface_T;
      Col : Color_Table;
      Pixel_Size : Natural;
      Input_Stream : Ada.Streams.Stream_IO.Stream_Access) is
      
      Row_Size    : constant Integer_32 := 0; -- TODO
      -- Rows have a 32 bits padding
      Row_Padding : constant Integer_32 := (32 - (Row_Size mod 32)) mod 32 / 8;

   begin
      null; -- TODO
   end Read_Surface_Data;
   
   function Get (File_Name : String) return Surfaces.Surface_T is
      File : File_Type;
   begin
      -- Open file, read BMP info, close file
      -- TODO
      return S : Surfaces.Surface_T (1 .. 0, 1 ..0);
   exception
      when others =>
         -- release all resources, and re-raise
         -- TODO
         return S : Surfaces.Surface_T (1 .. 0, 1 ..0);
   end Get;
   
   function Get (File : File_Type) return Surface_T
   is
      Input_Stream : Ada.Streams.Stream_IO.Stream_Access
        := null; -- TODO: Open stream

      Hdr : Header;
      Inf : Info;
      Col : Color_Table;
      
      Width : Column_T;
      Height : Row_T;
   
   begin
      U8_Array'Read (Input_Stream, Hdr.Arr);
      U8_Array'Read (Input_Stream, Inf.Arr);
      -- TODO
      Col := (others => <>);
      return S : Surface_T (1 .. 0, 1 .. 0);
   end Get;

end BMP_File_IO;
