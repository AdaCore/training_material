package body Password_Manager is

   Passwords : array (Login_T) of Password_T;

   function Update
     (Login : Login_T)
      return Password_T is (Passwords (Email));
   function View
     (Login : Login_T)
      return Password_T is (Passwords (Email));

end Password_Manager;
