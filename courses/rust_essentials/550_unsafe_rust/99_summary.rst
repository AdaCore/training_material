=========
Summary
=========

--------------------------
Recap: Five Superpowers
--------------------------

- Dereference raw pointers
- Call unsafe functions or methods
- Access or modify mutable statics
- Implement unsafe traits
- Access union fields

-----------------
What We Covered
-----------------

* **Safe vs. Unsafe**

  - Unsafe Rust introduces explicit safety obligations
  - Unchecked safety requirements must be upheld

* **Raw Pointers**

  - May be null, dangling, misaligned, or invalid
  - Creation is safe; dereferencing is unsafe

* **Unsafe Functions and Traits**

  - Callers uphold documented safety contracts
  - An :rust:`unsafe impl` promises required invariants

* **Unions**

  - Fields share storage
  - Reading a union field requires :rust:`unsafe`

* **Safe Abstractions**

  - Keep unsafe operations small and auditable
  - Check invariants before exposing a safe API
  - Prefer Safe Rust when it can express the behavior
