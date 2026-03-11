================
Program Memory
================

----------
Overview
----------

**Stack**
  - Continuous area of available memory
  - Values have fixed sizes known at compile time
  - Extremely fast due to contiguous memory layout

**Heap**
  - Allocation is requested at runtime
  - Supports dynamic sizes and data that must outlive a function call
  - Slower due to pointer indirection and allocation overhead

-----------------------
Memory Layout Example
-----------------------

- Creating a :rust:`String` puts 
  - Fixed-sized metadata on the stack 
  - Dynamically sized data (actual string) on the heap

.. code:: rust

       let s1 = String::from("Hello");

.. image:: comprehensive_rust_training/review_of_program_memory.svg

