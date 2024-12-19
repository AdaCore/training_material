=========
Summary
=========

------------------------------------------
Properties of Discriminated Record Types
------------------------------------------

* Rules

   - Case choices for variants must partition possible values for discriminant
   - Field names must be unique across all variants

* Style

   - Typical processing is via a case statement that "dispatches" based on discriminant
   - This centralized functional processing is in contrast to decentralized object-oriented approach
