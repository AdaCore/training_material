===================
Type-Specific I/O
===================

------------------------
Ada.Text_IO.Integer_IO
------------------------

.. code:: Ada

   declare
      type Integer_T is range -1_000 .. 1_000;
      package Io is new Ada.Text_IO.Integer_IO (Integer_T);
      I : Integer_T;
   begin
      Io.Get (I);
      Io.Put
        (Item  => I,
         Width => 10,  -- optional: minimum number of characters to print
         Base  => 16); -- optional: numeric base
   end;

* `Get` will read until a non-numeric character is encountered, ignoring leading or trailing whitespace

   - **123** will set I to 123
   - **45X67** will set I to 45

* :ada:`IO` has global objects :ada:`Default_Width` and :ada:`Default_Base` which can be modified to set default values for like-named parameters
* :ada:`Ada.Text_IO.Modular_IO` behaves the same

------------------------
Ada.Text_IO.Float_IO
------------------------

.. code:: Ada

   declare
      type Float_T is digits 6 range -100.0 .. 100.0;
      package Io is new Ada.Text_IO.Float_IO (Float_T);
      F : Float_T;
   begin
      Io.Get (F);
      Io.Put
        (Item => F,
         Fore => 1,   -- optional: number of digits before decimal point
         Aft  => 2,   -- optional: number of digits after decimal point
         Exp  => 3);  -- optional: numeric of characters for exponent
   end;

* `Get` will read until a non-numeric character is encountered, ignoring leading or trailing whitespace

   - **12** will set F to 12.0
   - **23.45.67** will set F to 23.45

* :ada:`IO` has global objects :ada:`Default_Fore`, :ada:`Default_Aft` and :ada:`Default_Exp` which can be modified to set default values for like-named parameters
* `Ada.Text_IO.Fixed_IO` and `Ada.Text_IO.Decimal_IO` behave the same

----------------------------
Ada.Text_IO.Enumeration_IO
----------------------------

.. code:: Ada

   declare
      type Enumeration_T is (Red, Yellow, Green);
      package Io is new Ada.Text_IO.Enumeration_IO (Enumeration_T);
      E : Enumeration_T;
   begin
      Io.Get (E);
      Io.Put
        (Item => F,
         Width => 10,          -- optional: minimum number of characters to print
         Set   => Lower_Case); -- optional: flag for Upper_Case or Lower_Case
   end;

* `Get` will read until the end of the line or trailing whitespace, case-insensitive

   - **YelloW** will set `E` to `Yellow`
   - **Red Blue** will set `E` to `Red`

* :ada:`IO` has global objects :ada:`Default_Width` and :ada:`Default_Setting` which can be modified to set default values for like-named parameters

