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
   :O'Range: another way of saying :ada:`O'First` .. :ada:`O'Last`

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
