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

