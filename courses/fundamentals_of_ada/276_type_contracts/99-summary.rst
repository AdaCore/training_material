=========
Summary
=========

------------------------------
Working with Type Invariants
------------------------------

* They are not fully foolproof

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
