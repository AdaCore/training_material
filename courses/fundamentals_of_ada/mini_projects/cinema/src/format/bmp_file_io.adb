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
   --  +---+---+---+----------+
   --  | R | G | B | Reserved |
   --  +---+---+---+----------+
   --
   -- If there is no palette, pixels are directly stored, as either
   -- 4 bytes (Pixel_Size = 32).
   -- This is the case for the dummy files, and for the rotating_triangle movie.
   --
   --  +---+---+---+----------+
   --  | R | G | B | Reserved |
   --  +---+---+---+----------+
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
      Input_Stream : Ada.Streams.Stream_IO.Stream_Access);
     --  Read the content of an input stream of bytes that is the pixels data part of
     --  a BMP file and convert it to a surface data, according to the given color table,
     --  and pixel size.

   procedure Read_Surface_Data
     (Surf : in out Surface_T;
      Col : Color_Table;
      Pixel_Size : Natural;
      Input_Stream : Ada.Streams.Stream_IO.Stream_Access) is
      
      -- TODO: calculate the size of a row of pixels
      Row_Size    : constant Integer_32 := 0;
      -- Rows have a 32 bits padding
      Row_Padding : constant Integer_32 := (32 - (Row_Size mod 32)) mod 32 / 8;

   begin
      -- TODO
      -- Be careful of several things:
      -- 1. the rows are stored for the bottom one to the top one
      -- 2. the color data is not necessarily used, depending on the
      --    size of the color data table (0 or not)
      -- 3. even though the color index may be of several sizes, you can
      --    safely assume it is on 8 bits, using a Unsigned_8
      null;
   end Read_Surface_Data;
   
   function Get (File : File_Type) return Surface_T
   is
      -- cannot read from a file type, directly, need
      -- to have a stream from it
      Input_Stream : Ada.Streams.Stream_IO.Stream_Access
        := null; -- TODO: Turn file to a stream

      Hdr : Header;
      Inf : Info;
      Col : Color_Table;
      
      Width : Column_T;
      Height : Row_T;
   
   begin
      -- Reading BMP header
      U8_Array'Read (Input_Stream, Hdr.Arr);
      -- Reading BMP info table
      U8_Array'Read (Input_Stream, Inf.Arr);
      -- TODO
      -- The color table is stored with a size given
      -- in the info table
      Col := (others => <>);

      -- TODO
      -- Verify the header's and info table's content
      -- signature should be as per the BMP spec above
      -- supported compression scheme are 0 or 3
      -- supported pixel sizes are: 4 bytes, 3 bytes,
      --    or else 1 byte (index to the palette)

      -- TODO
      -- Jump to the proper pixel data offset (see in Header)
      -- TODO
      -- Read the file's BMP pixels raw data, and convert those
      -- to a Surface
      return S : Surface_T (1 .. 0, 1 .. 0);
   end Get;

   function Get (File_Name : String) return Surfaces.Surface_T is
      File : File_Type;
   begin
      -- Open file, read BMP info, close file
      -- TODO
      -- Open the file, read its content as a surface,
      -- close the file.
      -- In order to read the content, re-use the Get from a
      -- stream you have already implemented above.
      return S : Surfaces.Surface_T (1 .. 0, 1 ..0);
   exception
      when others =>
         -- release all resources, and re-raise
         -- TODO
         -- In case of error, we want to close the
         -- opened file, and re-raise the exception
         return S : Surfaces.Surface_T (1 .. 0, 1 ..0);
   end Get;

end BMP_File_IO;
