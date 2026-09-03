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

   - **Compile-time:** specific constructs, features, and rules
   - **Run-time:** language-defined and user-defined exceptions

* Facilities as part of the language definition

   - Range specifications
   - Parameter modes
   - Generic contracts
   - Work well, but on a restricted set of use-cases

* Contract aspects to be more expressive

   - Carried by subprograms
   - ... or by types (seen later)
   - Can have **arbitrary** conditions, more **versatile**
   - Failure to meet a contract is an :dfn:`assertion failure`

     - Typically causes an :ada:`Assertion_Error` exception to be raised
