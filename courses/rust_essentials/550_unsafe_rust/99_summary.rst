=========
Summary
=========

-----------------
What We Covered
-----------------

* **Safe / Unsafe Split**

  - Unsafe Rust adds five explicit operations
  - Unchecked safety requirements must be upheld

* **Five Superpowers**

  - Dereference raw pointers
  - Call unsafe functions or methods
  - Access or modify mutable statics
  - Implement unsafe traits
  - Access union fields

* **Raw Pointers**

  - May be null, dangling, misaligned, or invalid
  - Creation is safe; dereferencing is unsafe

* **Unsafe Functions and Traits**

  - Callers uphold documented safety contracts
  - An :rust:`unsafe impl` promises required invariants

* **Safe Abstractions**

  - Keep unsafe operations small and auditable
  - Check invariants before exposing a safe API
  - Prefer Safe Rust when it can express the behavior
