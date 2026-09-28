==============
Introduction
==============

----------------
Topics Covered
----------------

- **Heap allocation**

  - Flexible sizing
  
  - Working with dynamically sized types

- **Dereferencing**

  - Overriding the operator
  
  - Transparent data access via coercion
  
- **Shared Ownership**
  
  - Reference counting

---------------------
Why Smart Pointers?
---------------------

- Store data on the heap
  - Useful when values should not live inline

- Allow recursive types
  - Give recursive fields a known size

- Allow multiple owners
  - Share ownership of data for complex architectures

