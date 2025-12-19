====================
Contracts by Cases
====================

-------------------------------------
Simplifying Pre- and Postconditions
-------------------------------------

* Assume you have a subprogram that increments away from zero

  .. code:: Ada

    procedure Away_From_Zero (Number : in out Integer) with
      Post =>
       (if Number'Old < 0 then Number = Number'Old - 1
        elsif Number'Old > 0 then Number = Number'Old + 1
        else Number = Number'Old);

* You are really trying to show different expected outputs over the
  entire input range

  * Almost like a :ada:`case` expression!

----------------------
Contract Cases (1/2)
----------------------

* SPARK defines aspect :ada:`Contract_Cases`

  - Syntax of named aggregate
  - Each case consists of a guard (left-hand side) and a consequence (right-hand side)
  - Inspired by *Parnas Tables*

* Simplified version of :ada:`Away_From_Zero`

  .. code:: ada

    procedure Away_From_Zero (Number : in out Integer) with
      Contract_Cases =>
        (Number < 0 => Number = Number'Old - 1,
         Number > 0 => Number = Number'Old + 1,
         Number = 0 => Number = Number'Old);

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

