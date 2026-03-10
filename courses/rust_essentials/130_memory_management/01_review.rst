================
Program Memory
================

--------------------------
Review of Program Memory
--------------------------

**Stack**
  - Continuous area of memory available
  - Values have fixed sizes known at compile time
  - **Pros:** Extremely fast and great memory locality

**Heap**
  - Storage of values outside of function calls
  - Values have dynamic sizes determined at runtime
  - **Cons:** Slighly slower and no guarantuee of locality

-----------------------
Memory Layout Example
-----------------------

- Creating a :rust:`String` puts 
  - Fixed-sized metadata on the stack 
  - Dynamically sized data, the actual string, on the heap

.. code:: rust

       let s1 = String::from("Hello");

.. image:: comprehensive_rust_training/review_of_program_memory.svg

