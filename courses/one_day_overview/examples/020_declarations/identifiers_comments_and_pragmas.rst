.. code:: ada
    :class: ada-syntax-only

   package Identifiers_Comments_And_Pragmas is
   
      Spaceperson : Integer;
      --SPACEPERSON : integer; -- identifier is a duplicate
      Space_Person : Integer;
      --Null : integer := 0; -- identifier is a reserved word
      pragma Unreferenced (Spaceperson);
      pragma Unreferenced (Space_Person);
   
   end Identifiers_Comments_And_Pragmas;
