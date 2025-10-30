package Password_Manager is

   type Login_T is (Email, Banking, Amazon, Streaming);
   type String_Access_T is access String;
   type Password_T is record
      Count    : Natural;
      Password : String_Access_T;
   end record;

   type Password_Access_T is access Password_T;

   function Update
     (Login : Login_T)
      return Password_Access_T;
   function View
     (Login : Login_T)
      return Password_T;

end Password_Manager;
