package Program_Practice is

   type T_Index_Type is range 1 .. 10;
   Anonymous_Array : array (T_Index_Type) of Integer :=
     (1 => 1, 2 => 3, others => -1);

   function Test
     (Flag1 : in Boolean;
      Flag2 : in Boolean)
      return Boolean;

   function Test
     (Flag : Character)
      return Character;

end Program_Practice;
