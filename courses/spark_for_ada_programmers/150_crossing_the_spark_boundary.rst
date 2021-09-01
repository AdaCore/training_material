
*****************************
Crossing the SPARK boundary
*****************************
.. |rightarrow| replace:: :math:`\rightarrow`

==============
Introduction
==============

-----------------------------
Crossing the SPARK boundary
-----------------------------

* A temporary step outside
* A well defined interface for more extensive interworking

   - With a foreign programming language component
   - For example a library or another subcomponent of the system

* Both crossings of the SPARK boundary usually make use of pragma or aspect `SPARK_Mode`

============
SPARK Mode
============

--------------
`SPARK_Mode`
--------------

* The `SPARK_Mode` aspect has been present on the examples and exercises during the course

   .. code:: Ada

      package Device with SPARK_Mode is

   + This is an abbreviation for `SPARK_Mode => On`

      - We have also seen the converse

      .. code:: Ada

         procedure Print_Board with SPARK_Mode => Off Is

   + There is an equivalent pragma as an aspect cannot always be used

      .. code:: Ada

         pragma SPARK_Mode (On); -- equivalent to SPARK_Mode => On
         pragma SPARK_Mode (Off); -- equivalent to SPARK_Mode => Off

--------------------------
`SPARK_Mode` Not Present
--------------------------

* If `SPARK_Mode` is not present on a compilation unit

   - Then the unit is not fully analyzed - it is checked for globals
   - However any SPARK compatible declarations in the unit may be used in the SPARK code

      .. code:: Ada

         package No_SPARK_Mode is
           type T is range 0.. 100;
           type Access_T is access T; -- Not allowed in SPARK
         end No_SPARK_Mode;

   - This unit's declarations can be denoted in SPARK code provided they are SPARK compatible, but use declarations of subprograms with caution - they may have globals which are not apparent

      .. code:: Ada

         with No_SPARK_Mode;
         procedure Use_No_SPARK_Mode with SPARK_Mode is
           My_Var : No_SPARK_Mode.T; -- This is ok
         begin
           My_Var := 0;
         end Use_No_SPARK_Mode;

--------------------------------
`SPARK_Mode` Off for Unit Spec
--------------------------------

* If `SPARK_Mode` is off for a unit specification

   - Then the unit is not analyzed, and it cannot be used in SPARK code

      .. code:: Ada

         package SPARK_Mode_Off with SPARK_Mode => Off is
         type T is range 0.. 100;
         type Access_T is access T;
         end SPARK_Mode_Off;

   - If we try to use this unit we get errors

      .. code:: Ada

         with SPARK_Mode_Off;
         procedure Use_SPARK_Mode_Off with SPARK_Mode is
           My_Var : SPARK_Mode_Off.T;
         begin
           My_Var := 0;
         end Use_SPARK_Mode_Off;

      .. code:: console

         use_spark_mode_off.adb:5:13:
             "T" is not allowed in SPARK
         use_spark_mode_off.adb:5:13:
             violation of aspect SPARK_Mode at line 3

--------------------------------
`SPARK_Mode` Off for Unit Body
--------------------------------

* If `SPARK_Mode` is off for a unit body

   - Then the unit body is checked for globals unless it is a subprogram body whose declaration has a `Global` contract
   - If the specification (declaration) of the unit is in SPARK then its declaration can be used in SPARK code

* If `SPARK_Mode` is on for unit

   - Then the unit is analyzed
   - All the code of the compilation unit must be in SPARK (apart from enclosed units specified with `SPARK_Mode => Off`)
   - Of course, the unit can be used in SPARK code

-----------------------
What Can It Apply To?
-----------------------

* `SPARK_Mode` may apply to:

   - A unit specification (declaration)
   - A unit body
   - A private part of a package (`pragma SPARK_Mode` must used here)
   - The sequence of statements of a package body (`pragma SPARK_Mode` must used here)

* `SPARK_Mode` is a also a configuration pragma

   - This means it can be placed in a configuration file and apply to all compilation units

-------------------------
Where Can It Be Placed?
-------------------------

* `SPARK_Mode` can only be placed at library-level
* If `pragma SPARK_Mode` is used rather than aspect for a unit

   - It is placed before any context clause, or,
   - Immediately after the `is` of a package declaration or body, or,
   - Immediately after a subprogram declaration or after the `is` of a subprogram body

* `SPARK_Mode` for a private part

   - The `pragma` is placed immediately after the word `private`

* For the sequence of statements of a package body

   - `SPARK_Mode` pragma is placed immediately after the word `begin`

------------
Principles
------------

* Apply `SPARK_Mode => On`

   - To all units you want to analyze with the SPARK tools

* Use `SPARK_Mode  => Off` with caution

   - Moving a private part of out of SPARK allows the declaration of a type which is not in SPARK but it also allows many other things which could invalidate the analysis and proof
   - If the body of a unit is not in SPARK it is important that the SPARK contract on the specification correctly models the essential behavior of the body, :toolname:`GNATprove` cannot check this
   - When the sequence of statements of a package body has `SPARK_Mode` Off, :toolname:`GNATprove` assumes that the `Initializes` and `Initial_Condition` contracts are satisfied by the statements; ensure this is the case and they do not do anything else

========
Lab
========

.. include:: labs/150_crossing_the_spark_boundary.lab.rst

=========
Summary
=========

---------
Summary
---------

* Our entire project cannot be SPARK

   + At least not yet!
   + Interactions with the outside world can't always conform to the necessary requirements

* How do we use SPARK as much as possible?

   + Interfaces can be SPARK while implementations are not
   + We can even make part of the implementation SPARK
