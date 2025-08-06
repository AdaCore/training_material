====================
Contracts by Cases
====================

----------------------
Contract Cases (1/2)
----------------------

* Some contracts are best expressed by cases

  - Inspired by *Parnas Tables*

* SPARK defines aspect :ada:`Contract_Cases`

  - Syntax of named aggregate
  - Each case consists of a guard and a consequence

* Example from SPARK tutorial

  .. code:: ada

     Contract_Cases =>
       (A(1) = Val                              => ...
        Value_Found_In_Range (A, Val, 2, 10)    => ...
        (for all J in Arr'Range => A(J) /= Val) => ...

----------------------
Contract Cases (2/2)
----------------------

* :toolname:`GNATprove` checks that **each** case holds

  - When guard is enabled on entry, consequence holds on exit
  - Note: guards are evaluated **on entry**
  - Attributes :ada:`Old` and :ada:`Result` allowed in consequence

|

* :toolname:`GNATprove` checks that cases are **disjoint** and **complete**

  - All inputs allowed by the precondition are covered by a single case

|

* When enabled at run-time:

  - Run-time check that exactly one guard holds on entry
  - Run-time check that the corresponding consequence hold on exit

-------------------
Exceptional Cases
-------------------

* Needed when exception propagation is expected

  .. code:: ada

     -- Constraint error in specific case
     Exceptional_Cases => 
        (Constraint_Error => Status = Error);
    
     -- All exceptions (most general form)
     Exceptional_Cases => (others => True);

* Different exceptions can be grouped by cases

  .. code:: ada

     Exceptional_Cases =>
       (Constraint_Error | Numerical_Error => Post1,
        Program_Error                      => Post2);

* :toolname:`GNATprove` checks that **each** case holds

  - When exception is raised, consequence holds on exit
  - Attribute :ada:`Old` allowed in consequence

* No run-time effect

