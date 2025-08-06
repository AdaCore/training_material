=========
Summary
=========

----------------
Advanced Proof
----------------

* Use relaxed initialization when needed

  - Some variables are partially initialized
  - Some array variables are initialized in a loop
  - More annotations are needed with ghost attribute :ada:`Initialized`

* Proof of loops requires more work

  - Add loop invariants to prove correction
  - Take special care of the loop frame condition
  - Add loop variants to prove termination

* Formal containers

  - Generics for vectors, lists, sets and maps
  - Available in all runtime libraries
  - Proof of code using formal containers uses formal models
