=========
Summary
=========

------------------------------
Working with Type Invariants
------------------------------

* They are not completely foolproof

   - External corruption is possible
   - Requires dubious usage

* Violations are intended to be supplier bugs

   - But not necessarily so, since not always bullet-proof

* However, reasonable designs will be foolproof

-------------------------------
Type Invariants Vs Predicates
-------------------------------

* Type Invariants are valid at external boundary

   - Useful for complex types - type may not be consistent during an operation

* Predicates are like other constraint checks

   - Checked on declaration, assignment, calls, etc

-------------------------------------
Contract-Based Programming Benefits
-------------------------------------

* Facilitates building software with reliability built-in

   - Software cannot work well unless "well" is carefully defined
   - Clarifies design by defining obligations/benefits

* Enhances readability and understandability

   - Specification contains explicitly expressed properties of code

* Improves testability but also likelihood of passing!
* Aids in debugging
* Facilitates tool-based analysis

   - Compiler checks conformance to obligations
   - Static analyzers (e.g., SPARK, GNAT Static Analysis Suite) can verify explicit preconditions and postconditions
