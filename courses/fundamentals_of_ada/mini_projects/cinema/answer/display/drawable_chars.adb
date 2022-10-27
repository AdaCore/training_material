with Ada.Strings.Unbounded;

package body Drawable_Chars is

   function Image (C : Drawable_Char_T) return String is
   begin
      return String (C);
   end Image;

   function Debug_Image (C : Sorted_Charset_T) return String is
      use Chars_Sorted_By_Characteristic_Pkg;
      use Ada.Strings.Unbounded;

      It : Sorted_Charset_Cursor_T := C.First;
      S  : Unbounded_String        := Null_Unbounded_String;
   begin

      while Has_Element (It) loop
         declare
            El : constant Char_With_Characteristic_T := Element (It);
         begin
            S :=
              S & """" & Image (El.Char.all) & """ => " & El.Value'Image & " ";
            It := Next (It);
         end;
      end loop;

      return To_String (S);
   end Debug_Image;

   function Closest
     (Metric : Sorted_Charset_T; Value : Drawable_Char_Characteristic_T)
      return Drawable_Char_T
   is
      use Chars_Sorted_By_Characteristic_Pkg;
      It   : Sorted_Charset_Cursor_T    := Metric.First;
      Best : Char_With_Characteristic_T := Element (It);

      Best_Dist : Natural := Natural (abs (Best.Value - Value));
      Continue  : Boolean := True;
   begin
      while Continue loop
         declare
            Curr : constant Char_With_Characteristic_T := Element (It);
         begin
            if Curr.Value >= Value then
               declare
                  Curr_Dist : constant Natural := Natural (Curr.Value - Value);
               begin
                  if Curr_Dist < Best_Dist then
                     Best_Dist := Curr_Dist;
                     Best := Curr;
                  end if;
               end;

               Continue := False;
            else
               Best     := Curr;
               It       := Next (It);
               Continue := Has_Element (It);
            end if;
         end;
      end loop;

      return Best.Char.all;
   end Closest;

   function Sort_By
     (Charset        : Drawable_Charset_T;
      Characteristic : Drawable_Char_Caracteristics_List_T)
      return Sorted_Charset_T
   is
   begin
      return SC : Sorted_Charset_T do
         for J in Charset'Range loop
            SC.Insert ((Char => Charset (J), Value => Characteristic (J)));
         end loop;
      end return;
   end Sort_By;

   function Reversed (SC : Sorted_Charset_T) return Sorted_Charset_T is
      use Chars_Sorted_By_Characteristic_Pkg;
      It  : Sorted_Charset_Cursor_T := SC.First;
      SC2 : Sorted_Charset_T;
   begin
      while Has_Element (It) loop
         declare
            El : constant Char_With_Characteristic_T := Element (It);
         begin
            SC2.Insert ((El.Char, 255 - El.Value));
         end;
         It := Next (It);
      end loop;
      return SC2;
   end Reversed;

end Drawable_Chars;
