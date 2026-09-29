=========
Summary
=========

-------------------------
Recap: Five Superpowers
-------------------------

#. Dereference raw pointers
#. Call unsafe functions or methods
#. Access or modify mutable statics
#. Implement unsafe traits
#. Read union fields

-----------------
What We Covered
-----------------

* **Safe vs. Unsafe**

  - Unsafe Rust introduces explicit safety obligations
  - Safety requirements the compiler cannot verify must be upheld

* **Raw Pointers**

  - May be null, dangling, misaligned, or invalid
  - Creation is safe; dereferencing is an unsafe operation

* **Unsafe Functions and Traits**

  - Callers uphold documented safety contracts
  - An :rust:`unsafe impl` promises that required safety conditions are met

* **Unions**

  - Fields share storage
  - Reading a union field is an unsafe operation

* **Safe Abstractions**

  - Keep unsafe operations small and auditable
  - Check safety conditions before exposing a safe API
  - Prefer Safe Rust when it can express the behavior
