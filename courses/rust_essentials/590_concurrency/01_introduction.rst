==============
Introduction
==============

--------------------------------------------------
Concurrency - One Program, Many Things Happening
--------------------------------------------------

* **Sequential programs** - one instruction at a time

* **Concurrent programs** - multiple activities *conceptually* happening at once

  * Even on one CPU
  * Think many cooks in one kitchen

* Why concurrency?

  * Respond to external events (real-time systems)
  * Improve performance or responsiveness

---------------------
Concurrency in Rust
---------------------

* Built-in language constructs



-----------------------
Issue of Shared State
-----------------------

* Hardware interrupts act as preemptive concurrent threads
* Global mutable variables (:rust:`static mut`) cause data races
* Rust forbids aliasing mutable references

.. code:: rust

    // Inherently unsafe: ISR can interrupt read/write
    static mut COUNTER: u32 = 0;

* Solution: Interior mutability with static types that implement ``Sync``


-----------------------------
Lock-Free State via Atomics
-----------------------------

* ``core::sync::atomic`` types implement ``Sync``.
* Mutation occurs through shared references (``&self``).
* Directly compiles to atomic CPU instructions (no interrupt masking needed).

.. code-block:: rust

    use core::sync::atomic::{AtomicBool, Ordering};

    static EVENT_FIRED: AtomicBool = AtomicBool::new(false);

    // In ISR:
    EVENT_FIRED.store(true, Ordering::Relaxed);

    // In Main Loop:
    if EVENT_FIRED.swap(false, Ordering::Relaxed) {
        // Handle event
    }


----------------------------
Critical Sections & Unsafe
----------------------------

* Atomics are limited to simple scalar primitives.
* Complex data structures require disabling interrupts.
* ``critical-section`` provides safe synchronization by masking preemption:

.. code-block:: rust

    use core::cell::RefCell;
    use critical_section::Mutex;

    static BUFFER: Mutex<RefCell<Vec<u8, 32>>> = Mutex::new(RefCell::new(Vec::new()));

    critical_section::with(|cs| {
        BUFFER.borrow(cs).borrow_mut().push(0x42);
    });

* Under the hood, disabling interrupts requires ``unsafe`` assembly.

----------------------------
std vs. no_std Concurrency
----------------------------
.. list-table::
   :header-rows: 1

   * - Feature
     - std
     - no_std (Bare-Metal)
   * - Unit
     - OS Thread
     - ISR / Async Task
   * - Mutex Action
     - Sleeps thread (OS scheduler)
     - Masks interrupts / Spinlock
   * - Allocation
     - Dynamic (``Arc<Mutex<T>>``)
     - Static (``'static``)


----------------------------------------
Async Rust: Multitasking Without an OS
----------------------------------------

* No operating system or RTOS scheduler required.
* Functions compiled into zero-cost state machines.
* Driven by a hardware-aware executor:

.. code-block:: text

   [ Task yields at .await ] ---> [ Executor calls WFI (Sleep) ]
                                            |
   [ Task resumes execution ] <--- [ ISR calls Waker.wake() ]


----------------------------------
Memory Efficiency: RTOS vs Async
----------------------------------

* **RTOS / Threads**:
  * Each task requires a dedicated stack (e.g., 2 KB each).
  * 10 tasks = 20 KB RAM allocated up-front.
  * Risk of stack overflow.

* **Async Rust**:
  * All tasks share the single main stack during execution.
  * Paused tasks store only state machine variants.
  * Static footprint known completely at compile time.