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

----------------------------------
Example for Attribute 'Old (1/3)
----------------------------------

* We have a procedure that replaces digits in a string with "*"

  * Replace the character at the current index with "*"
  * Increment the current index

  .. code:: Ada

    Text  : String(1..5) := "12-45";
    Index : Integer := 1;
    procedure Sanitize_Digit;

* We add a postcondition to ensure correct behavior

  .. code:: Ada

    procedure Sanitize_Digit
      with Post =>
        not (Is_Digit (Text(Index)'Old))
        or  Text(Index'Old) = '*';

  * Check that character used to be a digit
  * If yes, character is now a "*"

----------------------------------
Example for Attribute 'Old (2/3)
----------------------------------

* But we have no guarantee that :ada:`Index` is in range

  * So modify the postcondition to check it

  .. code:: Ada

    procedure Sanitize_Digit
      with Post =>
        (if Index in Text'Range then
          not (Is_Digit (Text(Index)'Old))
          or  Text(Index'Old) = '*');

* But this won't fix the problem!

  * :ada:`Text(Index)'Old` is evaluated on **entry**
  * What happens when :ada:`Index` is out of range?

* One possible solution 

  .. code:: Ada

    procedure Sanitize_Digit
      with Post =>
        (if Index'Old in Text'Range then
          not (Is_Digit (Text'Old(Index'Old)))
          or  Text(Index'Old) = '*');

----------------------------------
Example for Attribute 'Old (3/3)
----------------------------------


**Using** :ada:`'Old` **wisely to examine a character**

  .. list-table::
    :widths: 40 10 50

    * - :ada:`Text (Index)'Old`
      - ``1``
      - *Indexed character value on entry*

    * - :ada:`Text'Old (Index'Old)`
      - ``1``
      - *Use entry index and entry text*

    * - :ada:`Text (Index'Old)`
      - ``*``
      - *Use entry index and current text*

    * - :ada:`Text (Index)`
      - ``2``
      - *Uses current index and current text*

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


