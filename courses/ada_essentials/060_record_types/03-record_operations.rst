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
      Phase1 := Phase2;  -- entire object reference
      -- component references
      Phase1.Real := 2.5;
      Phase1.Real := Phase2.Real;
   end;

-----------------------------
Limited Types - Quick Intro
-----------------------------

* A :ada:`record` type can be limited

    - And some other types, described later

* :dfn:`limited` types cannot be **copied** or **compared**

    - As a result then cannot be assigned
    - May still be modified component-wise

.. code:: Ada

    type Lim is limited record
        A, B : Integer;
    end record;

    L1, L2 : Lim := Create_Lim (1, 2); -- Initial value OK

    L1 := L2; -- Illegal
    if L1 /= L2 then -- Illegal
    [...]

