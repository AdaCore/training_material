=====================
Hardware Interrupts
=====================

---------------------------
Hardware-Level Preemption
---------------------------

* Hardware interrupts are fully supported in no_std environments

  * Can be paired with external architecture crates

    * :rust:`cortex-m-rt`, :rust:`riscv-rt`, or :rust:`avr-device`

* Interrupts are triggered directly by hardware peripherals

  * CPU forcibly pauses the current execution
  * Instantly jumps execution to an Interrupt Service Routine
  * Runs in a special hardware execution mode

    * Not within an OS thread or async task

* Higher-priority interrupts preempt lower-priority ones

-------
Usage
-------

**ISRs are written as top-level functions**

  * So the hardware interrupt vector table can call them
  * :rust:`#[no_mangle]` keeps the name intact for the linker

.. code:: rust

  #[no_mangle]
  // 1. Hardware Interrupt Handler
  pub fn on_cat_jumped() {
    deploy_emergency_catnip();
  }

  // 2. Foreground Main Loop
  fn main() {
    loop {
      write_safe_rust_code();
    }
  }

----------------------------
Global Shared State Hazard
----------------------------

* Interrupt can trigger at any CPU instruction

* Sharing state between main and an ISR creates corruption


.. code:: rust

  // DANGER: Global mutable state
  static mut COUNTER: u32 = 0; //

  fn main() -> ! {
    unsafe {
        COUNTER += 1; // Requires multi-step CPU Read-Modify-Write
    }
    loop {}
  }

  fn TIMER0() {
    unsafe {
        // If main is interrupted mid-write above, COUNTER becomes corrupted!
        COUNTER = 0;
    }
  }

* Standard global variables require :rust:`static mut`

  * Inherently :rust:`unsafe` because ISR can interrupt a read/write

----------------
Using "atomic"
----------------

**Atomic types (** :rust:`core::sync::atomic` **) resolve issues without** :rust:`unsafe` **or** :rust:`static mut`

  * Use *Interior Mutability*

  * Mutate the underlying value through shared immutable reference (:rust:`&self`)

  * Implement the :rust:`Sync` trait

.. code:: rust

  use core::sync::atomic::{AtomicBool, Ordering};

  // Shared static atomic flag
  static EVENT_FIRED: AtomicBool = AtomicBool::new(false); //

  // Hardware Interrupt Service Routine (ISR)

  #[no_mangle]
  pub fn TIMER0() {
    // Atomically set flag to true without needing locks or unsafe blocks
    EVENT_FIRED.store(true, Ordering::Relaxed); //
  }

  fn main() {
    // Atomically check and reset flag back to false
    if EVENT_FIRED.swap(false, Ordering::Relaxed) { //
        // Process event safely
    }
  }


-----------------
Memory Ordering
-----------------

:rust:`atomic` **operation takes an** :rust:`Ordering` **parameter**

.. code:: rust

  #[non_exhaustive]
  pub enum Ordering {
    Relaxed,
    Acquire,
    Release,
    AcqRel,
    SeqCst,
  }

  * Controls how hardware and compiler balance performance and synchronization guarantees

-------------------------------
"atomic" Types and Operations
-------------------------------


.. list-table::
   :header-rows: 1
   :stub-columns: 1

   * - **Type**
     - **Under the Hood**
     - **Common Use Cases**
   * - :rust:`AtomicBool`
     - 1-byte hardware flag
     - Flags, signals, trigger
   * - :rust:`AtomicUsize` / :rust:`AtomicIsize`
     - Pointer-sized integer
     - Shared reference counters
   * - :rust:`AtomicU32` / :rust:`AtomicI32`
     - 32-bit primitive
     - Statistics, global counters
   * - :rust:`AtomicPtr<T>`
     - Raw pointer (:rust:`*mut T`)
     - Lock-free data structures

.. list-table::
   :header-rows: 1
   :stub-columns: 1

   * - **Operations**
     - **Usage**
   * - :rust:`.store(val, ordering)`
     - writes a new value
   * - :rust:`.load(ordering)`
     - reads the current value
   * - :rust:`.swap(val, ordering)`
     - writes new value and returns old value
   * - :rust:`.fetch_add(val, ordering)`
     - adds to current value and returns previous