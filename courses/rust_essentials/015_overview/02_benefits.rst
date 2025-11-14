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

----------------------
Rust Compared to ...
----------------------

- Ada

  - Similar focus on reliability, performance and low-level control

  - Safety through runtime checks and very strong type system

  - Rust's borrow checker statically prevents memory errors that Ada
    would typically catch at runtime

- C or C++

  - Eliminates a whole class of *runtime errors* via the borrow checker

    - Performance without memory safety issues

  - Constructs like pattern matching and built-in dependency management

- Java, Go, Python, JavaScript

  - Same memory safety
  - Fast and predictable performance like C and C++

    - No garbage collector
    - Access to low-level hardware (should you need it)
