with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Logs is

   subtype Color is Style range Black .. None;
   
   type Style_Content_T is record
      Foreground, Background : Color := None;
   end record;
   
   type Styles_Array_T is array (Style) of Style_Content_T;
   Styles_Colors : constant Styles_Array_T
     := (
         Black => (Black, None),
         Red => (Red, None),
         Green => (Green, None),
         Yellow => (Yellow, None),
         Blue => (Blue, None),
         Magenta => (Magenta, None),
         Cyan => (Cyan, None),
         White => (White, None),
         Dummy => (None, None),
         None => (None, None),
         Black_On_Red => (Black, Red),
         White_On_Red => (White, Red)
        );
   
   type Escaped_VT100 is new String;
   
   function VT100_Esc (Content : String) return Escaped_VT100 is
   begin
      return Character'Val (8#33#) & "[" & Escaped_VT100 (Content) & "m";
   end VT100_Esc;
   
   function VT100_Esc (Colors : Style_Content_T) return Escaped_VT100 is
      function To_VT100 (I : Integer) return String is
         S : constant String := I'Image;
      begin
         return S (S'First + 1 .. S'Last);
      end To_VT100;
   begin
      return VT100_Esc (To_VT100 (30 + Color'Pos (Colors.Foreground))
                        & ";"
                        & To_VT100 (40 + Color'Pos (Colors.Background)));
   end VT100_Esc;
   
   function VT100 (Escaped : Escaped_VT100; S : String) return String is
   begin
      return String (Escaped) & S & String (VT100_Esc ("0"));
   end VT100;
   
   function VT100 (Colors : Style_Content_T; S : String) return String is
   begin
      return VT100 (VT100_Esc (Colors), S);
   end VT100;
   
   function VT100 (Sty : Style; S : String) return String is
   begin
      return VT100 (Styles_Colors (Sty), S);
   end VT100;
   
   Buffered : Boolean := False;
   package Log_Buffer_Pkg
     is new Ada.Containers.Vectors (Index_Type   => Positive,
                                    Element_Type => Unbounded_String);
   
   B : Log_Buffer_Pkg.Vector := Log_Buffer_Pkg.Empty_Vector;
   B_Current_Line : Unbounded_String := Null_Unbounded_String;
   
   procedure Buffer is
   begin
      Buffered := True;
   end Buffer;
   
   procedure Unbuffer is
   begin
      Buffered := False;
      Put_Buffer;
   end Unbuffer;
   
   procedure Put_Buffer is
      Old_Buffered : constant Boolean := Buffered;
   begin
      Buffered := False;
      for Line of B loop
         Put_Line (To_String (Line));
      end loop;
      
      if B_Current_Line /= "" then
         Put (To_String (B_Current_Line));
      end if;
      
      Log_Buffer_Pkg.Clear (B);
      Set_Unbounded_String (B_Current_Line, "");
      
      Buffered := Old_Buffered;
   end Put_Buffer;
   
   procedure Drop_Buffer is
   begin
      Log_Buffer_Pkg.Clear (B);
      Set_Unbounded_String (B_Current_Line, "");
   end Drop_Buffer;
   
   procedure Put (Sty : Style; S : String) is
   begin
      Put (VT100 (Sty, S));
   end Put;

   procedure Put_Line (Sty : Style; S : String) is
   begin
      Put_Line (VT100 (Sty, S));
   end Put_Line;
   
   procedure Put (S : String) is
   begin
      if Buffered then
         Append (B_Current_Line, S);
      else
         Ada.Text_IO.Put (S);
      end if;
   end Put;
   
   procedure Put_Line (S : String) is
   begin
      Put (S);
      New_Line;
   end Put_Line;
   
   procedure New_Line is
   begin
      if Buffered then
         Log_Buffer_Pkg.Append (B, B_Current_Line);
         B_Current_Line := Null_Unbounded_String;
      else
         Ada.Text_IO.New_Line;
      end if;
   end New_Line;
end Logs;
