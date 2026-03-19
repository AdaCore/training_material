==============
Introduction
==============

----------------
Topics Covered
----------------

- **Flexible Sizing**

  - Heap allocation
  
  - Bypassing static size constraints

- **Seamless Interaction**

  - Overriding the dereference operator
  
  - Transparent data access via coercion

- **User-Defined Smart Pointers**

  - Automatic resource management
  
  - Order of destruction and explicit cleanup

- **Shared Responsibility**

  - Shared ownership of heap data
  
  - Tracking active references

---------------------
Why Smart Pointers?
---------------------

- *Ownership is Strict!*

  - One owner and strict borrowing makes for complex patterns

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

- Access data via automatic dereferencing
