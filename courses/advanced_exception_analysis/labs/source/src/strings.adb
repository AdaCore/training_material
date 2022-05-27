with Ada.Strings.Fixed; use Ada.Strings.Fixed;
package body Strings is

   function Convert
     (Str : String_T)
      return String is (Str.all);
   function Convert
     (Str : String)
      return String_T is (new String'(Str));

   function Split
     (Str       : String;
      Separator : String := "|")
      return Strings_T is
      Sep  : constant Integer := Index (Str, Separator);
      This : String_T;
   begin
      if Sep in Str'range
      then
         This := new String'(Str (Str'first .. Sep - 1));
         return This & Split (Str (Sep + 1 .. Str'last), Separator);
      else
         This := new String'(Str);
         declare
            Retval : Strings_T (1 .. 1) := (others => This);
         begin
            return Retval;
         end;
      end if;
   end Split;

end Strings;
