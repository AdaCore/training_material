==================
Benefits of Rust
==================

----------------------------
Compile Time Memory Safety
----------------------------

Whole classes of memory bugs are prevented at compile time

   -  No uninitialized variables
   -  No double-frees
   -  No use-after-free
   -  No :rust:`NULL` pointers
   -  No forgotten locked mutexes
   -  No data races between threads
   -  No iterator invalidation

-------------------------------
No Undefined Runtime Behavior
-------------------------------

What a Rust statement does is never left unspecified

   -  Array access is bounds checked
   -  Integer overflow is defined (panic or wrap-around)

--------------------------
Modern Language Features
--------------------------

As expressive and ergonomic as other higher-level languages

   -  Enums and pattern matching
   -  Generics
   -  Zero-cost Foreign Function Interface (FFI)
   -  Zero-cost abstractions
   -  Great compiler errors
   -  Built-in dependency manager
   -  Built-in support for testing
   -  Excellent Language Server Protocol support
   -  OOP-style power without the class-hierarchy baggage

--------------------------------
Rust in the Language Ecosystem
--------------------------------

- **vs. Ada**

  - Similar goals: reliability, performance, low-level control

  - Also enforces many safety guarantees at *compile* time

  - Borrow checker enforces strict ownership and aliasing rules

- **vs. C/C++**

  - Memory safety by default

  - C-like performance *without* manual memory management

  - Modern conveniences missing in C/C++

- **vs. Java**

  - Memory safety *without* a garbage collector

  - No null references

  - Predictable performance and direct hardware access
