   type My_Int_T is private
      with Integer_Literal => Make_0;

   function Make_0 (S : String) return My_Int_T;
   ...
   type My_Int_T is record
       I : Integer;
   end record;

   function Make_0 (S : String) return My_Int_T is ((I => 0));
