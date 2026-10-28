==============
Introduction
==============

--------------------------------------------------
Concurrency - One Program, Many Things Happening
--------------------------------------------------

* **Sequential programs**

  * One instruction at a time

* **Concurrent programs**

  * Multiple activities *conceptually* happening at once
  * Even on one CPU
  * Think many cooks in one kitchen

* Why concurrency?

  * Respond to external events (real-time systems)
  * Improve performance or responsiveness

---------------------
Concurrency in Rust
---------------------

**Built-in language constructs**

  * Threads (:rust:`std::thread`)

    * Spawn OS-level threads running true parallel instructions

  * Hardware Interrupts / ISR

    * Hardware events pause execution and jump to ISR (:rust:`#[interrupt]`)
    * Thread-safe non-blocking mutation types (:rust:`core::sync::atomic`)

  * Cooperative Multitasking

    * Compile asynchronous control flow (:rust:`async`/ :rust:`.await`)
    * Traits dictate types can be moved/ shared across boundaries (:rust:`Send`/ :rust:`Sync`)

