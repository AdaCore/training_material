=========================
Constrained Array Types
=========================

-------------------------------------
Constrained Array Type Declarations
-------------------------------------

* Syntax

      .. code:: Ada

         constrained_array_definition ::=
            array index_constraint of subtype_indication
         index_constraint ::= (discrete_subtype_definition
            {, discrete_subtype_indication})
         discrete_subtype_definition ::=
            discrete_subtype_indication | range
         subtype_indication ::= subtype_mark [constraint]
         range ::= range_attribute_reference |
            simple_expression .. simple_expression

* Examples

   .. code:: Ada

      type Full_Week_T is array (Days) of Float;
      type Work_Week_T is array (Days range Mon .. Fri) of Float;
      type Weekdays is array (Mon .. Fri) of Float;
      type Workdays is array (Weekdays'Range) of Float;

----------------------------------
Multiple-Dimensioned Array Types
----------------------------------

.. container:: columns

 .. container:: column

    * Declared with more than one index definition

       - Constrained array types
       - Unconstrained array types

    * Components accessed by giving value for each index

 .. container:: column

   .. container:: latex_environment small

    .. code:: Ada

       type Three_Dimensioned is
         array (
           Boolean,
           12 .. 50,
           Character range 'a' .. 'z')
           of Integer;
         TD : Three_Dimensioned;
         ...
       begin
         TD (True, 42, 'b') := 42;
         TD (Flag, Count, Char) := 42;

-----------------------------
Tic-Tac-Toe Winners Example
-----------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       -- 9 positions on a board
       type Move_Number is range 1 .. 9;
       -- 8 ways to win
       type Winning_Combinations is
          range 1 .. 8;
       -- need 3 positions to win
       type Required_Positions is
          range 1 .. 3;
       Winning : constant array (
          Winning_Combinations,
          Required_Positions)
          of Move_Number := (1 => (1,2,3),
                             2 => (1,4,7),
                             ...

 .. container:: column

    .. list-table::
       :width: 55%

      * - :superscript:`1` **X**

        - :superscript:`2` **X**
        - :superscript:`3` **X**

      * - :superscript:`4`

        - :superscript:`5`
        - :superscript:`6`

      * - :superscript:`7`

        - :superscript:`8`
        - :superscript:`9`

      * -

        -
        -

      * - :superscript:`1` **X**

        - :superscript:`2`
        - :superscript:`3`

      * - :superscript:`4` **X**

        - :superscript:`5`
        - :superscript:`6`

      * - :superscript:`7` **X**

        - :superscript:`8`
        - :superscript:`9`

      * -

        -
        -

      * - :superscript:`1` **X**

        - :superscript:`2`
        - :superscript:`3`

      * - :superscript:`4`

        - :superscript:`5` **X**
        - :superscript:`6`

      * - :superscript:`7`

        - :superscript:`8`
        - :superscript:`9` **X**

------
Quiz
------

.. code:: Ada

   type Array1_T is array (1 .. 8) of Boolean;
   type Array2_T is array (0 .. 7) of Boolean;
   X1, Y1 : Array1_T;
   X2, Y2 : Array2_T;

.. container:: columns

 .. container:: column

   Which statement(s) is (are) legal?

   A. :answermono:`X1 (1) := Y1 (1);`
   B. :answermono:`X1 := Y1;`
   C. :answermono:`X1 (1) := X2 (1);`
   D. ``X2 := X1;``

 .. container:: column

  .. container:: animate

    Explanations

    A. Legal - elements are :ada:`Boolean`
    B. Legal - object types match
    C. Legal - elements are :ada:`Boolean`
    D. Although the sizes are the same and the elements are the same, the type is different

