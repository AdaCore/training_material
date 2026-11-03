=========
Summary
=========

-----------------
What We Covered
-----------------

* **Why** :rust:`no_std`?

  - Removes automatic dependency on :rust:`std` for a crate


* **Programming with** :rust:`core`

  - Provides types, traits, iterators, and formatting

  - Borrowed data, return values, and caller-supplied interfaces work without :rust:`std`


* **Optional Allocation**

  - Adds heap-backed types when final program supports allocation


* **Dependencies and Features**

  - Cargo features control optional capabilities

  - Reusable libraries can support core-only, allocating, and :rust:`std` configurations


* **Checking Configurations**

  - Feature configurations and intended targets require separate checks

  - Host checks alone do not prove target portability
