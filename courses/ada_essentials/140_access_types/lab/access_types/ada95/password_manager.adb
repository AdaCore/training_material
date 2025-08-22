package body Password_Manager is

   Passwords : array (Login_T) of aliased Password_T;

   function Update
     (Login : Login_T)
      return Modifiable_T is
   begin
      return Passwords (Login)'Access;
   end Update;
   function View
     (Login : Login_T)
      return Viewable_T is
   begin
      return Passwords (Login)'Access;
   end View;

end Password_Manager;
