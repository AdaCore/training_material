==============
Introduction
==============

----------------
Topics Covered
----------------

- **Heap allocation**

  - Flexible sizing
  
  - Bypassing static size constraints

- **Dereferencing**

  - Overriding the operator
  
  - Transparent data access via coercion
  
- **Shared Ownership**
  
  - Tracking references

- **User-Defined Smart Pointers**

  - Custom behaviors
  
  - Order of destruction and explicit cleanup

---------------------
Why Smart Pointers?
---------------------

- Enable recursive types

  - Provide a fixed-size pointer on the stack

- Prevent stack overflows

  - Allocate data on the heap

- Allow multiple owners

  - Share ownership of data for complex architectures

