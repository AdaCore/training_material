.. code:: ada
    :class: ada-run

   with Ada.Calendar; use Ada.Calendar;
   package Object_Declarations is
      A    : Integer := 0;
      B, C : Time    := Clock;
      D    : Integer := A + 1;
   end Object_Declarations;
