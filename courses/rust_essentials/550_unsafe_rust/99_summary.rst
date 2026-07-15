=========
Summary
=========

-----------------
What We Covered
-----------------

* **The Safe / Unsafe Split**

  - Unsafe Rust adds a small, explicit set of operations
  - The programmer must uphold the corresponding safety contracts

* **The Five Unsafe Superpowers**

  - Dereference raw pointers
  - Call unsafe functions or methods
  - Access or modify mutable statics
  - Implement unsafe traits
  - Access union fields

* **Raw Pointers**

  - Raw pointers may be null, dangling, or incorrectly aligned
  - Creating them is safe; dereferencing them requires an unsafe block

* **Unsafe Functions and Traits**

  - Callers of an unsafe function must satisfy its documented preconditions
  - An :rust:`unsafe impl` promises that required invariants are upheld

* **Safe Abstractions**

  - Keep unsafe operations small and auditable
  - Validate all required invariants before exposing a safe interface
  - Prefer Safe Rust whenever it can express the required behavior
