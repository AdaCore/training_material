package Password_Manager is

   type Login_T is (Email, Banking, Amazon, Streaming);
   type Password_String_T is access String;
   type Password_T is record
      Count    : Natural;
      Password : Password_String_T;
   end record;

   type Modifiable_T is access all Password_T;
   type Viewable_T is access constant Password_T;

   function Update (Login : Login_T) return Modifiable_T;
   function View (Login : Login_T) return Viewable_T;

end Password_Manager;
