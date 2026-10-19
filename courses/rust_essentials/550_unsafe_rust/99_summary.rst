=========
Summary
=========

-----------------
What We Covered
-----------------

* **Safe vs. Unsafe**

  - Unsafe Rust introduces explicit safety obligations
  - Safety requirements the compiler cannot verify must be upheld

* **Raw Pointers**

  - Creation is safe
  - Dereferencing requires :rust:`unsafe`

* **Unsafe Functions**

  - Callers are responsible for satisfying safety contracts

* **Mutable Statics**

  - Reading or writing :rust:`static mut` requires :rust:`unsafe`

* **Unsafe Traits**

  - Implementors are responsible for satisfying safety contracts

* **Unions**

  - Reading a union field requires :rust:`unsafe`

* **Safe Abstractions**

  - Encapsulate unsafe operations behind a sound, safe API
