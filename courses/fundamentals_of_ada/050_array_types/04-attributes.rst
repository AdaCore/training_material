============
Attributes
============

------------------
Array Attributes
------------------

* Return info about array index bounds

   :O'Length: number of array components
   :O'First: value of lower index bound
   :O'Last: value of upper index bound
   :O'Range: another way of saying :ada:`T'First` .. :ada:`T'Last`

* Meaningfully applied to constrained array types

   - Only constrained array types provide index bounds
   - Returns index info specified by the type (hence all such objects)

* Meaningfully applied to array objects

   - Returns index info for the object
   - Especially useful for objects of unconstrained array types

----------------------
Attributes' Benefits
----------------------

* Allow code to be more robust

   - Relationships are explicit
   - Changes are localized

* Optimizer can identify redundant checks

   .. code:: Ada

      declare
         type Int_Arr is array (5 .. 15) of Integer;
         Vector : Int_Arr;
      begin
         ...
         for Idx in Vector'Range loop
            Vector (Idx) := Idx * 2;
         end loop;

  * Compiler understands :ada:`Idx` has to be a valid index for :ada:`Vector`, so no run-time checks are necessary

--------------------------------
Nth Dimension Array Attributes
--------------------------------

* Attribute with **parameter**

.. code:: Ada

  T'Length (n)
  T'First (n)
  T'Last (n)
  T'Range (n)

- ``n`` is the dimension

  + defaults to 1

.. code:: Ada

   type Two_Dimensioned is array
      (1 .. 10, 12 .. 50) of T;
   TD : Two_Dimensioned;

* :ada:`TD'First (2) = 12`
* :ada:`TD'Last  (2) = 50`
* :ada:`TD'Length (2) = 39`
* :ada:`TD'First = TD'First (1) = 1`

------
Quiz
------

.. code:: Ada

   subtype Index1_T is Integer range 0 .. 7;
   subtype Index2_T is Integer range 1 .. 8;
   type Array_T is array (Index1_T, Index2_T) of Integer;
   X : Array_T;

Which comparison is False?

   A. ``X'Last (2) = Index2_T'Last``
   B. :answermono:`X'Last (1)*X'Last (2) = X'Length (1)*X'Length (2)`
   C. ``X'Length (1) = X'Length (2)``
   D. ``X'Last (1) = 7``

.. container:: animate

   Explanations

   A. 8 = 8
   B. 7*8 /= 8*8
   C. 8 = 8
   D. 7 = 7

