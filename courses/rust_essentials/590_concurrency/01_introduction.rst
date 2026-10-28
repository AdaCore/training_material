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

**Different mechanisms**

  * Threads (:rust:`std::thread`)

    * Spawn OS-level threads running true parallel instructions

  * Hardware Interrupts / ISR

    * Hardware events pause execution and jump to ISR 

  * Cooperative Multitasking

    * Compile asynchronous control flow (:rust:`async`/ :rust:`.await`)

**Built-in language constructs**
    
  * Traits dictate types can be moved/ shared across boundaries (:rust:`Send`/ :rust:`Sync`)
  * Thread-safe non-blocking mutation types (:rust:`core::sync::atomic`)

----------------
Topics Covered
----------------

- **Threads (**:rust:`std::thread`**)**
  - OS-managed scheduling and preemption
  - Joining and transferring ownership with :rust:`move`

- **The** :rust:`Send` & :rust:`Sync` **Traits**
  - Compiler auto-traits for boundary safety
  - Moving ownership vs. sharing references across execution contexts

- **Hardware Interrupts (ISRs)**
  - Bare-metal hardware preemption
  - Safe global communication using :rust:`core::sync::atomic`

- **Cooperative Multitasking (**:rust:`async` **/** :rust:`.await`**)**
  - Single-stack state machines without OS overhead
  - Driving embedded tasks with executors (:rust:`embassy`)

- **State Sharing & Synchronization**
  - Interior mutability primitives (:rust:`Cell`, :rust:`RefCell`)
  - OS-level :rust:`Mutex<T>` vs. bare-metal :rust:`critical_section`
  