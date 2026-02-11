============
Approaches
============

---------------------------------
Approaches to Memory Management
---------------------------------

- Manual Memory Management (e.g., C)
  - **Control:** Full control. Programmer decides when to allocate/free.
  - **Risk:** Programmer must ensure pointers are valid. High risk of human error.
- Automatic Memory Management (e.g., Java, Python)
  - **Safety:** Runtime system (Garbage Collector) ensures memory is not freed until unreferenced.
  - **Cost:** Runtime overhead for reference counting or garbage collection.
- Rust's Approach
  - **Control & Safety:** Compile-time enforcement of correct memory management.
  - **Mechanism:** Uses the concept of **Ownership**.
