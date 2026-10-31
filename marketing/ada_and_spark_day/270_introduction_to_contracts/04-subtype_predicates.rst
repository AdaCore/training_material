====================
Subtype Predicates
====================

------------
Predicates
------------

* Assertion expected to hold for all objects of given type
* Expressed as any legal boolean expression in Ada

   - Quantified and conditional expressions
   - Boolean function calls

* Two forms in Ada

   - **Static Predicates**

      + Specified via aspect named :ada:`Static_Predicate`

   - **Dynamic Predicates**

      + Specified via aspect named :ada:`Dynamic_Predicate`

* Can apply to :ada:`type` or :ada:`subtype`

----------------------------
Subtype Predicate Examples
----------------------------

* Dynamic Predicate

  .. code:: Ada

     subtype Even is Integer with Dynamic_Predicate =>
        Even mod 2 = 0; -- Boolean expression
        -- (Even indicates "current instance")

* Static Predicate

  .. code:: Ada

     type Serial_Baud_Rate is range 110 .. 115200
       with Static_Predicate => Serial_Baud_Rate  in
         -- Non-contiguous range
         110  | 300  | 600 | 1200 | 2400 | 4800 |
         9600 | 14400 | 19200 | 28800 | 38400 | 56000 |
         57600 | 115200;

--------------------
Predicate Checking
--------------------

* Calls inserted automatically by compiler
* Violations raise exception :ada:`Assertion_Error`

   - When predicate does not hold (evaluates to False)

* Checks are done before value change

   - Same as language-defined constraint checks

* Associated variable is unchanged when violation is detected
