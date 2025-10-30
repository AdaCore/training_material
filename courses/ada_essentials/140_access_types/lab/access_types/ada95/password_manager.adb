package body Password_Manager is

   Passwords : array (Login_T) of aliased Password_Access_T;

   function Update
     (Login : Login_T)
      return Password_Access_T is
   begin
      if Passwords (Login) = null then
         Passwords (Login) := new Password_T;
      end if;
      return Passwords (Login);
   end Update;

   function View
     (Login : Login_T)
      return Password_T is
   begin
      return Passwords (Login).all;
   end View;

end Password_Manager;
