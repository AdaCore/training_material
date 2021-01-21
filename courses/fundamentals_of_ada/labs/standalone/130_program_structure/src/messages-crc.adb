
function Messages.Crc
  (Content : Content_T)
   return Crc_T is
   Ret_Val : Crc_T := 1;
begin
   for C of Content
   loop
      Ret_Val := Ret_Val * Character'Pos (C);
   end loop;
   return Ret_Val;
end Messages.Crc;
