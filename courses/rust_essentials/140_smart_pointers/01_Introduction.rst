==============
Introduction
==============

----------------
Topics Covered
----------------

- **Heap allocation**

  - Flexible sizing
  
  - Bypassing static size constraints

- **Dereferencing Smart Pointers**

  - Overriding the operator
  
  - Transparent data access via coercion
  
- **Shared Ownership**
  
  - Tracking active references

- **User-Defined Smart Pointers**

  - Custom behaviors of smart pointers
  
  - Order of destruction and explicit cleanup

---------------------
Why Smart Pointers?
---------------------

- To move big datas to the Heap

- And recursivity!

- Rust solution is smart pointers

  - Help navigate Rust strictness

  - Use internal logic to safely provide flexibility where references cannot
  
    - Heap indirection
  
    - Reference counting

    - And more!	

- Common references (:rust:`&T`) **point** to data 

  - *Smart Pointers* **own** it

- Mostly implemented as *Generic Structs* to wrap any type

- Memory is freed when no longer needed

