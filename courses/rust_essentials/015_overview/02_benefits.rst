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

- **Ada**

  - **Rust** offers similar reliability, performance, and low-level control

  - **Rust** enforces many safety guarantees *at compile time* rather than via runtime checks

  - **Rust’s** borrow checker prevents memory errors that Ada typically detects only at runtime

- **C/C++**

  - **Rust** eliminates entire classes of memory-related runtime errors common in C/C++

    - Provides C-like performance *without* memory-safety risks

  - **Rust** adds modern conveniences missing in C/C++
  
    - Pattern matching
    - Built-in dependency management

- **Java**

  - **Rust** provides memory safety *without* a garbage collector
  - **Rust** avoids the “null reference” problem entirely (a major source of Java runtime bugs)
  - **Rust** delivers fast, predictable performance and access to low-level hardware when needed
