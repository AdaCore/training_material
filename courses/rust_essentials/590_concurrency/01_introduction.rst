==============
Introduction
==============

----------------
Topics Covered
----------------

- **Hardware Interrupts**

  - Hardware preemption
  - Interleaving risks

- **Sharing States**

  - Enforce concurrency boundaries with traits and types

- **Atomic**

  - Atomic operations
  - Protecting shared data

- **Cooperative Multitasking**

  - Asynchronous tasks
  - Voluntarily yielding

--------------------------------------------------
Concurrency - One Program, Many Things Happening
--------------------------------------------------

* Sequential programs

  * One instruction at a time

* Concurrent programs

  * Multiple activities *conceptually* happening at once
  
    * Even on one CPU
	
  * Think many cooks in one kitchen

* Why concurrency?

  * Respond to external events (real-time systems)
  * Improve performance or responsiveness

---------------------
Concurrency in Rust
---------------------

* Rust has built-in language constructs for

  * Hardware Interrupts
  * Cooperative Multitasking
  * Threads
   
* Traits for moving/sharing across boundaries
* Atomic types for thread-safe operations on shared data


