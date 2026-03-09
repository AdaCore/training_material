============
Approaches
============

---------------------------------
Approaches to Memory Management
---------------------------------

- Manual Memory Management (e.g., C/C++)
  - **Full Control**: Programmer decides when to allocate/deallocate
  - **Risk**: Programmer must ensure pointers are valid, risk of human error
- Automatic Memory Management (e.g., Java, Python)
  - **Safety**: Garbage Collector ensures memory is not freed until unreferenced
  - **Cost**: Runtime overhead for reference counting or garbage collection
- Rust's Approach
  - **Control & Safety**: Compile-time memory guarantees
  - **Mechanism**: Uses the concept of **Ownership**
