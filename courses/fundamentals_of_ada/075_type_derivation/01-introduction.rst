==============
Introduction
==============

-----------------
Type Derivation
-----------------

* Type :dfn:`derivation` allows for reusing code
* Type can be **derived** from a **base type**
* Base type can be substituted by the derived type
* Subprograms defined on the base type are **inherited** on derived type
* This is **not** OOP in Ada

    - Tagged derivation **is** OOP in Ada

---------------------------
Reminder: What is a Type?
---------------------------

* A type is characterized by two components

   - Its data structure
   - The set of operations that applies to it

* The operations are called **primitive operations** in Ada

.. container:: latex_environment small

   .. code:: Ada

      package Types is
         type Integer_T is range -(2**63) .. 2**63-1 with Size => 64; 
         procedure Increment_With_Truncation (Val : in out Integer_T);
         procedure Increment_With_Rounding (Val : in out Integer_T);
     end Types;

