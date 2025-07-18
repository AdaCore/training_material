=========================
Constrained Array Types
=========================

-------------------------------------
Constrained Array Type Declarations
-------------------------------------

Syntax (simplified)

.. container:: latex_environment footnotesize

   .. code:: Ada

      type <typemark> is array (<index constraint>) of <constrained type>;

where

.. container:: latex_environment quote

      **typemark** - identifier

      **index constraint** - discrete range or type

      **constrained type** - type with size known at compile time

Examples

.. container:: latex_environment footnotesize

   .. code:: Ada

      type Integer_Array_T is array (1 .. 3) of Integer;
      type Boolean_Array_T is array (Boolean) of Integer;
      type Character_Array_T is array (character range 'a' .. 'z') of Boolean;
      type Copycat_T is array (Boolean_Array_T'Range) of Integer;

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

    A. Legal - components are :ada:`Boolean`
    B. Legal - object types match
    C. Legal - components are :ada:`Boolean`
    D. Although the sizes are the same and the components are the same, the type is different

