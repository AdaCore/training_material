==============
Introduction
==============

----------------
Topics Covered
----------------

- **Hardware Interrupts**

  - Understand hardware preemption and interleaving risks

- **Sharing States**

  - Enforce concurrency boundaries with :rust:`Send`, :rust:`Sync`, and :rust:`UnsafeCell`

- **Atomic**

  - Synchronize shared data with atomics and interrupt masking

- **Cooperative Multitasking**

  - Run concurrent tasks on a shared stack using :rust:`async` and :rust:`Future`

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

* Different mechanisms

  * Hardware Interrupts 

    * Hardware events pause execution and jump to handler 

  * Cooperative Multitasking

    * Asynchronous control flow 
	
  * Threads

    * Spawn OS-level threads

* Built-in language constructs
    
  * Traits for moving/sharing across boundaries 
  * Thread-safe non-blocking mutation types 


