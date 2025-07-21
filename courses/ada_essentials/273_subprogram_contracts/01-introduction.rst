==============
Introduction
==============

--------------------
Design-By-Contract
--------------------

* Source code acting in roles of **client** and **supplier** under a binding **contract**

   - :dfn:`Contract` specifies *requirements* or *guarantees*

      - *"A specification of a software element that affects its use by potential clients."* (Bertrand Meyer)

   - :dfn:`Supplier` provides services

       - Guarantees specific functional behavior
       - Has requirements for guarantees to hold

   - :dfn:`Client` utilizes services

       - Guarantees supplier's conditions are met
       - Requires result to follow the subprogram's guarantees

---------------
Ada Contracts
---------------

* Ada contracts include enforcement

   - At compile-time: specific constructs, features, and rules
   - At run-time: language-defined and user-defined exceptions

* Facilities as part of the language definition

   - Range specifications
   - Parameter modes
   - Generic contracts
   - OOP :ada:`interface` types
   - Work well, but on a restricted set of use-cases

* Contract aspects to be more expressive

   - Carried by subprograms
   - ... or by types (seen later)
   - Can have **arbitrary** conditions, more **versatile**

-----------
Assertion
-----------

* Boolean expression expected to be :ada:`True`
* Said *to hold* when :ada:`True`
* Language-defined :ada:`pragma`

.. code:: Ada

   pragma Assert (not Full (Stack));
   -- stack is not full
   pragma Assert (Stack_Length = 0,
                  Message => "stack was not empty");
   -- stack is empty

* Raises language-defined :ada:`Assertion_Error` exception if expression does not hold
* The :ada:`Ada.Assertions.Assert` subprogram wraps it

.. code:: Ada

   package Ada.Assertions is
     Assertion_Error : exception;
     procedure Assert (Check : in Boolean);
     procedure Assert (Check : in Boolean; Message : in String);
   end Ada.Assertions;
