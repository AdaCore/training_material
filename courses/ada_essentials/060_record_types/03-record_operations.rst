===================
Record Operations
===================

----------------------
Available Operations
----------------------

* Predefined

   - Equality (and thus inequality)

     .. code:: Ada

        if A = B then

  - Assignment

    .. code:: Ada

        A := B;

* User-defined

   - Subprograms

---------------------
Assignment Examples
---------------------

.. code:: Ada

   declare
     type Complex is record
         Real : Float;
         Imaginary : Float;
       end record;
     ...
     Phase1 : Complex;
     Phase2 : Complex;
   begin
     ...
       -- object reference
      Phase1 := Phase2;
      -- component references
      Phase1.Real := 2.5;
      Phase1.Real := Phase2.Real;
   end;
