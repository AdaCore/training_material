==============
Introduction
==============

----------------
Topics Covered
----------------

* **Peripheral Ownership**

  - Acquiring and moving peripheral handles
  - Limits of exclusive ownership

* **The Typestate Pattern**

  - State types, zero-sized types (ZSTs), and :rust:`PhantomData`

* **Building a GPIO API**

  - Unique handles and consuming transitions
  - Operations restricted by state

* **Typestate in Practice**

  - :rust:`embedded-hal` integration, type erasure, and trade-offs

---------------------
Learning Objectives
---------------------

**After this module, you should be able to**

* Explain how ownership manages peripheral handles

* Distinguish peripheral ownership from hardware configuration

* Use typestate to restrict which operations an API exposes

* Explain what a driver must guarantee behind a safe interface
