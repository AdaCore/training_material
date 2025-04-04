==============
Introduction
==============

--------------
Proof So Far
--------------

* Variables follow data initialization policy

  - Flow analysis deals with initialization
  - Arrays must be initialized by aggregates
  - Variables cannot be partially/conditionally initialized

* Loop-free code

  - Strongest Postcondition calculus does not deal with loops

    + At least, not without a little help

* How do we deal with the following program?

  .. code:: ada

     procedure Init_Table (T : out Table) is
     begin
        for J in T'Range loop
           T(J) := 0;
        end loop;
     end Init_Table;

.. container:: speakernote

   :ada:`T` is progressively initialized in a loop.
   The techniques we saw so far can't deal with this program.

--------------------------
Going Beyond Basic Proof
--------------------------

* Relaxed initialization

  - Ability to partially initialize variables
  - Proof deals with initialization of such variables

|

* Loop pragmas

  - Specialized pragmas to deal with loops in proof
  - Loop invariants provide the necessary help
  - Loop variants deal with loop termination

|

* SPARK formal containers

  - Dealing with loops over vectors, lists, sets and maps

