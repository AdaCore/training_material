====================
Special Attributes
====================

--------------------------------------------
Evaluate an Expression on Subprogram Entry
--------------------------------------------

* Postconditions may require knowledge of a subprogram's **entry context**

  .. code:: Ada

      procedure Increment (This : in out Integer)
       with Post => ??? -- how to assert incrementation of `This`?

* Language-defined attribute :ada:`'Old`
* Expression is **evaluated** at subprogram entry

   - After preconditions check
   - Makes a copy

        + May be expensive

   - Expression can be **arbitrary**

        + Typically :ada:`in out` parameters and globals

   .. code:: Ada

      procedure Increment (This : in out Integer) with
          Pre  => This < Integer'Last,
          Post => This = This'Old + 1;

----------------------------
Example for Attribute 'Old
----------------------------

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

-----------------------------------------
Error on Conditional Evaluation of 'Old
-----------------------------------------

* This code is **incomplete**

  .. code:: Ada
    :font-size: scriptsize

    procedure Shift_And_Advance (Index : in out Integer) with
      Post =>
        Global (Index)'Old = Global'Old (Index'Old) and
        Global (Index'Old) = Global (Index);

  * What happens when :ada:`Index` is not in range for :ada:`Global`?

* So we add a range check

  .. code:: Ada
    :font-size: scriptsize

    procedure Shift_And_Advance (Index : in out Integer) with
      Post =>
        (if Index in Global'Range then
           Global (Index)'Old = Global'Old (Index'Old) and
           Global (Index'Old) = Global (Index));

  * This code is still wrong!

    * :ada:`Global (Index)'Old` is evaluated on **entry**
    * :ada:`Index` is only being verified **after** the call

* (One) Correct solution

  .. code:: Ada
    :font-size: scriptsize

    procedure Shift_And_Advance (Index : in out Integer) with
      Post =>
        (if Index in Global'Range then
           Global'Old (Index) = Global'Old (Index'Old) and
           Global (Index'Old) = Global (Index));

  * Check at the old position now copies entire :ada:`Global`

-----------------------------------------
Postcondition Usage of Function Results
-----------------------------------------

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

      .. include:: ../examples/adv_270_subprogram_contracts/special_attributes_spec.rst

  .. container:: column

    .. container:: latex_environment tiny

      .. include:: ../examples/adv_270_subprogram_contracts/special_attributes_body.rst


