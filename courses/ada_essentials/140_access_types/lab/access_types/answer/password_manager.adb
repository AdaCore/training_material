package body Password_Manager is

   Passwords : array (Login_T) of aliased Password_T;

   function Update (Login : Login_T) return Modifiable_T is
      (Passwords (Login)'Access);
   function View (Login : Login_T) return Viewable_T is
      (Passwords (Login)'Access);

end Password_Manager;
