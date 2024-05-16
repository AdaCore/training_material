package body Strings is

   function "&"
     (L, R : String_T)
      return String_T is
    (From_String (To_String (L) & To_String (R)));

   function To_String
     (S : String_T)
      return String is
    (S.Text (1 .. S.Length));

   function From_String
     (S : String)
      return String_T is
      L      : constant Natural :=
               Integer'min (S'Length, Maximum_Length);
      Retval : String_T;
   begin
      Retval.Length        := L;
      Retval.Text (1 .. L) :=
          S (S'First .. S'First + L - 1);
      return Retval;
   end From_String;

end Strings;
