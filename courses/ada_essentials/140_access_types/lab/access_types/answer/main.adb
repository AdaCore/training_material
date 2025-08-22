with Ada.Text_IO;      use Ada.Text_IO;
with Password_Manager; use Password_Manager;
procedure Main is

   procedure Update (Which : Password_Manager.Login_T;
                     Pw    : String;
                     Count : Natural) is
   begin
      Update (Which).Password := new String'(Pw);
      Update (Which).Count    := Count;
   end Update;

begin
   Update (Email, "QWE!@#", 1);
   Update (Banking, "asd123", 22);
   Update (Amazon, "098poi", 333);
   Update (Streaming, ")(*LKJ", 444);

   for Login in Login_T'Range loop
      Put_Line
        (Login'Image & " => " & View (Login).Password.all &
         View (Login).Count'Image);
   end loop;
end Main;
