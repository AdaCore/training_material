====================
Special Attributes
====================

--------------------------------------------
Evaluate an Expression on Subprogram Entry
--------------------------------------------

* Post-conditions may require knowledge of a subprogram's **entry context**

.. container:: latex_environment small

  .. code:: Ada

      procedure Increment (This : in out Integer)
       with Post => ??? -- how to assert incrementation of "This"?

* Language-defined attribute :ada:`'Old`
* Expression is **evaluated** at subprogram entry

   - After pre-conditions check
   - Makes a copy

        + :ada:`limited` types are forbidden
        + May be expensive

   - Expression can be **arbitrary**

        + Typically :ada:`in out` parameters and globals

   .. code:: Ada

      procedure Increment (This : in out Integer) with
          Pre  => This < Integer'Last,
          Post => This = This'Old + 1;

------------------------------
Example for Attribute "'Old"
------------------------------

.. container:: latex_environment small

  .. code:: Ada

      Global : String := Init_Global;
      ...
      -- In Global, move character at Index to the left one position,
      -- and then increment the Index
      procedure Shift_And_Advance (Index : in out Integer) is
      begin
         Global (Index) := Global (Index + 1);
         Index          := Index + 1;
      end Shift_And_Advance;

* Note the different uses of `'Old` in the postcondition

.. container:: latex_environment footnotesize

  .. code:: Ada

     procedure Shift_And_Advance (Index : in out Integer) with Post =>
        -- Global (Index) before call (so Global and Index are original)
        Global (Index)'Old
           -- Original Global and Original Index
           = Global'Old (Index'Old)
        and
        -- Global after call and Index before call
        Global (Index'Old)
           -- Global and Index after call
           = Global (Index);

-------------------------------------------
Error on Conditional Evaluation of "'Old"
-------------------------------------------

* This code is **incorrect**

.. code:: Ada

  procedure Clear_Character (In_String : in out String;
                             At_Position : Positive)
     with Post => (if At_Position in In_String'Range
                   then In_String (At_Position)'Old = ' ');

* Copies :ada:`In_String (At_Position)` on entry

   - Will raise an exception on entry if :ada:`At_Position not in In_String'Range`
   - The postcondition's :ada:`if` check is not sufficient

* Solution requires a full copy of :ada:`In_String`

.. code:: Ada

  procedure Clear_Character (In_String : in out String;
                             At_Position : Positive)
     with Post => (if At_Position in In_String'Range
                   then In_String'Old (At_Position) = ' ');

-------------------------------------------
Postcondition Usage of Function Results
-------------------------------------------

* :ada:`function` result can be read with :ada:`'Result`

.. code:: Ada

  function Greatest_Common_Denominator (A, B : Positive)
    return Positive with
      Post =>  Is_GCD (A, B,
                       Greatest_Common_Denominator'Result);

------
Quiz
------

.. code:: Ada

   Database : String (1 .. 10) := "ABCDEFGHIJ";
   Index    : Integer := 4;
   -- Set the value for the component at position Index in
   -- array Database to Value and then increment Index by 1
   procedure Set_And_Move (Value :        Character;
                           Index : in out Index_T);
      with Post => ...

Given the following expressions, what is their value if they are evaluated in the postcondition
of the call :ada:`Set_And_Move ('X', Index)`?

.. container:: animate 2-

   .. image:: subprogram_contracts_special_attributes-legend.svg
      :width: 60%

.. container:: animate 1-

   * ``Database'Old (Index)``

.. container:: animate 2-

   .. image:: subprogram_contracts_special_attributes-answer1.svg
      :width: 80%

.. container:: animate 1-

   * ``Database (Index'Old)``

.. container:: animate 3-

   .. image:: subprogram_contracts_special_attributes-answer2.svg
      :width: 80%

.. container:: animate 1-

   * ``Database (Index)'Old``

.. container:: animate 4-

   .. image:: subprogram_contracts_special_attributes-answer3.svg
      :width: 80%

-------------------------------------
Stack Example (Spec with Contracts)
-------------------------------------

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: ada

        package Stack_Pkg is
          procedure Push (Item : in Integer) with
              Pre  => not Full,
              Post => not Empty and then Top = Item;
          procedure Pop (Item : out Integer) with
              Pre  => not Empty,
              Post => not Full and Item = Top'Old;
          function Pop return Integer with
              Pre  => not Empty,
              Post => not Full and Pop'Result = Top'Old;
          function Top return Integer with
              Pre => not Empty;
          function Empty return Boolean;
          function Full return Boolean;
        end Stack_Pkg;

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: ada

        package body Stack_Pkg is
          Values  : array (1 .. 100) of Integer;
          Current : Natural := 0;
          procedure Push (Item : in Integer) is
          begin
             Current         := Current + 1;
             Values(Current) := Item;
          end Push;
          procedure Pop (Item : out Integer) is
          begin
             Item    := Values(Current);
             Current := Current - 1;
          end Pop;
          function Pop return Integer is
             Item : constant Integer := Values(Current);
          begin
             Current := Current - 1;
             return Item;
          end Pop;
          function Top return Integer is
            (Values(Current));
          function Empty return Boolean is
            (Current not in Values'Range);
          function Full return Boolean is
            (Current >= Values'Length);
        end Stack_Pkg;

.. raw:: latex

  \vspace {5mm}
