======================
Anonymous Array Types
======================

-----------------------
Anonymous Array Types
-----------------------

.. container:: columns

 .. container:: column

    * Array objects need not be of a named type

       .. code:: Ada

          A : array (1 .. 3) of B;

    * Without a type name, no object-level operations

       - Cannot be checked for type compatibility
       - Operations on components are still ok if compatible

 .. container:: column

    .. code:: Ada

       declare
       -- These are not same type!
         A, B : array (Foo) of Bar;
       begin
         A := B;  -- illegal
         B := A;  -- illegal
         -- legal assignment of values
         A(J) := B(K);
       end;

