package Password_Manager is

   type Login_T is (Email, Banking, Amazon, Streaming);
   type Password_T is null record;
   --  Need components for count and password string

   --  These aren't right, but they compile!
   function Update
     (Login : Login_T)
      return Password_T;
   function View
     (Login : Login_T)
      return Password_T;

end Password_Manager;
