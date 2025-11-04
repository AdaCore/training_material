--|messages_helpers_begin
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
package body Messages is
   use type Interfaces.Unsigned_32;

   Global_Unique_Id : Interfaces.Unsigned_32 := 0;

   function Validate (Original : Message_T) return Boolean is
      Clean : Message_T := Original;
   begin
      Clean.Crc := 0;
      return Crc.Generate (Clean'Address, Clean'Size) = Original.Crc;
   end Validate;

   function Create
     (Kind : Kind_T; Value : Value_T; Text : Text_T) return Message_T
   is
      Retval : Message_T;
   begin
      Global_Unique_Id := Global_Unique_Id + 1;
      Retval :=
        (Unique_Id => Global_Unique_Id,
         Kind      => Kind,
         Value     => Value,
         Text      => Text,
         Crc       => 0);
      Retval.Crc := Crc.Generate (Retval'Address, Retval'Size);
      return Retval;
   end Create;

   type Overlay_T is array (1 .. Message_T'Size / 8) of Interfaces.Unsigned_8;
   function Convert is new Ada.Unchecked_Conversion (Message_T, Overlay_T);
   function Convert is new Ada.Unchecked_Conversion (Overlay_T, Message_T);

   Const_Filename : constant String := "message.txt";
--|messages_helpers_end

--|messages_begin
   procedure Write (Message : Message_T) is
      Overlay : constant Overlay_T := Convert (Message);
      File    : File_Type;
   begin
      Create (File, Out_File, Const_Filename);
      for I in Overlay'Range loop
         Put (File, Character'Val (Overlay (I)));
      end loop;
      New_Line (File);
      Close (File);
   end Write;

   procedure Read (Message : out Message_T; Valid : out Boolean) is
      Overlay : Overlay_T;
      File    : File_Type;
   begin
      Valid := False;
      Open (File, In_File, Const_Filename);
      declare
         Str : constant String := Get_Line (File);
      begin
         Close (File);
         for I in Str'Range loop
            Overlay (I) := Character'Pos (Str (I));
         end loop;
         Message := Convert (Overlay);
         Valid := Validate (Message);
      end;
   end Read;

   procedure Print (Prompt : String; Message : Message_T) is
   begin
      Put_Line (Prompt & ":");
      Put_Line
        ("  Id    => " & Interfaces.Unsigned_32'Image (Message.Unique_Id));
      Put_Line ("  Kind  => " & Kind_T'Image (Message.Kind));
      Put_Line ("  Value => " & Value_T'Image (Message.Value));
      Put_Line ("  Text  => " & Message.Text);
      Put_Line ("  CRC   => " & Crc.Crc_T'Image (Message.Crc));
   end Print;
end Messages;
--|messages_end
