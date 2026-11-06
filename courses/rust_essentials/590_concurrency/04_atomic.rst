========
Atomic
========

---------------------------
Safe Shared State in Core
---------------------------

* Atomic types resolve shared-state issues without locks or static mut

  * Part of the standard :rust:`core::sync::atomic` module
  * Use interior mutability to mutate values via shared immutable references (:rust:`&self`)
  * Implement the :rust:`Sync` marker trait so they can live in a plain :rust:`static`

* Safe for single-core bare-metal concurrency

  * Hardware executes operations indivisibly at the CPU instruction level
  * Preemption by an interrupt cannot observe or cause a half-written state

-------------------------------
Interior Mutability Landscape
-------------------------------

.. list-table::
   :header-rows: 1

   :class: tight-table

   * - **Mechanism**
     - **Safety Guarantee**
     - **Execution Cost**
     - **Sync Status**
   * - :rust:`Cell<T>`
     - Value copies and moves
     - Zero overhead
     - :rust:`!Sync`
   * - :rust:`RefCell<T>`
     - Dynamic borrow counter
     - Minor branch checking
     - :rust:`!Sync`
   * - :rust:`Atomic`
     - Hardware bus lock & CAS
     - Lock-free assembly
     - :rust:`Sync`
  
-------------------------------
"atomic" Types and Operations
-------------------------------

.. list-table::
   :header-rows: 1
   :stub-columns: 1

   * - **Type**
     - **Under the Hood**
     - **Common Embedded Use Cases**
   * - :rust:`AtomicBool`
     - 1-byte hardware flag
     - Hardware signaling, event pending flags
   * - :rust:`AtomicUsize` / :rust:`AtomicIsize`
     - Pointer-sized integer
     - Buffer indices, ring buffer pointers
   * - :rust:`AtomicU32` / :rust:`AtomicI32`
     - 32-bit primitive
     - Event counters, metrics, packed words
   * - :rust:`AtomicPtr<T>`
     - Raw pointer (:rust:`*mut T`)
     - Lock-free node links, buffer handoffs

.. list-table::
   :header-rows: 1
   :stub-columns: 1

   * - **Operation**
     - **Indivisible Action**
   * - :rust:`.store(val, ordering)`
     - Writes a new value into shared memory
   * - :rust:`.load(ordering)`
     - Reads the current value from shared memory
   * - :rust:`.swap(val, ordering)`
     - Writes new value and returns previous value
   * - :rust:`.fetch_add(val, ordering)`
     - Adds to current value and returns previous value
   * - :rust:`.compare_exchange(...)`
     - Updates value only if it matches expected value
	 

-----------------
Memory Ordering
-----------------

* Atomic operations take an :rust:`Ordering` parameter

  * Instructs the compiler and CPU how to reorder surrounding memory accesses
  * Select ordering based on what the atomic variable is communicating

* **Relaxed**

  * Guarantees atomicity for that specific memory word only
  * Establishes no synchronization or ordering guarantees across other variables
  * Correct choice when the atomic variable itself is the entire message

* **Release (Store)**

  * Ensures preceding memory writes are committed before this store is visible
  * Used by a producer to publish non-atomic or related data

* **Acquire (Load)**

  * Ensures subsequent memory reads observe changes made prior to matching release
  * Used by a consumer to acquire published data safely

* **AcqRel**

  * Combines Acquire and Release for read-modify-write operations (:rust:`.swap()`)

* **SeqCst**

  * Enforces a globally consistent order across all cores
  * Unneeded overhead for single-core
  

------------------------------
Target Hardware Capabilities
------------------------------

* Not every embedded microcontroller supports all atomic types

  * Architecture instruction set determines whether atomic operations exist
  * Smaller microcontrollers may lack 32-bit atomic instructions

* The compiler guards architecture capabilities via configuration flags

  * :rust:`#[cfg(target_has_atomic = "8")]` for 8-bit support
  * :rust:`#[cfg(target_has_atomic = "32")]` for 32-bit support


--------------------------
Atomic Read-Modify-Write
--------------------------

* Separate loads and stores reintroduce interleaving hazards

  * Loading an atomic and storing later is two distinct operations
  * An interrupt firing between them can erase notifications

.. code:: rust

  use core::sync::atomic::{AtomicBool, Ordering::Relaxed};

  static EVENT: AtomicBool = AtomicBool::new(false);

  // WRONG: Interrupt can fire between load and store, losing the event!
  fn take_wrong() -> bool {
      let pending = EVENT.load(Relaxed);
      EVENT.store(false, Relaxed);
      pending
  }

  // CORRECT: Read and clear execute as a single indivisible instruction
  fn take_correct() -> bool {
      EVENT.swap(false, Relaxed)
  }

-------------------------------
Event Flags vs Event Counters
-------------------------------

* Choose the atomic primitive based on communication requirements

* **Event Flags: AtomicBool**

  * Answers: "Did at least one event happen?"
  * Multiple rapid interrupts coalesce into a single notification flag
  * Ideal for work-needed notifications and wake-up triggers

* **Event Counters: AtomicU32**

  * Answers: "How many events occurred?"
  * Uses :rust:`.fetch_add(1, Relaxed)` so no occurrences are lost
  * Foreground uses :rust:`.swap(0, Relaxed)` to consume total batch count

.. code:: rust

  use core::sync::atomic::{AtomicU32, Ordering::Relaxed};

  static EVENT_COUNT: AtomicU32 = AtomicU32::new(0);

  // Hardware interrupt handler records every arrival
  fn on_uart_rx_interrupt() {
      EVENT_COUNT.fetch_add(1, Relaxed);
  }

  // Foreground loop processes exact number of accumulated events
  fn process_events() {
      let count = EVENT_COUNT.swap(0, Relaxed);
      for _ in 0..count {
          handle_single_event();
      }
  }


-------------------------------
One-Time Publication Protocol
-------------------------------

* Using Acquire and Release to synchronize access to other state

  * Value is written to shared memory first
  * Flag is published with Release to announce data readiness
  * Foreground checks flag with Acquire before reading payload

.. code:: rust

  use core::sync::atomic::{AtomicBool, AtomicU32, Ordering};

  static PAYLOAD: AtomicU32 = AtomicU32::new(0);
  static READY: AtomicBool = AtomicBool::new(false);

  // Writer publishes data before setting ready flag
  fn publish_sample(val: u32) {
      PAYLOAD.store(val, Ordering::Relaxed);
      READY.store(true, Ordering::Release);
  }

  // Reader only consumes payload after observing release flag
  fn read_sample() -> Option<u32> {
      if READY.load(Ordering::Acquire) {
          Some(PAYLOAD.load(Ordering::Relaxed))
      } else {
          None
      }
  }

---------------------------------------
Why "Volatile" Is Not Synchronization
---------------------------------------

* Developers frequently confuse volatile accesses with concurrency primitives

* **Volatile Accesses (:rust:`read_volatile` / :rust:`write_volatile`)**

  * Informs the compiler not to optimize out reads or writes to memory-mapped registers
  * Guarantees an access occurs, but provides zero cross-context synchronization
  * Does not prevent preemption or instruction reordering across execution contexts

* **Atomic Operations**

  * Designed specifically for concurrency between execution contexts
  * Guarantees indivisible updates and prevents memory corruption during preemption
  
-----------------------------
When Atomics Are Not Enough
-----------------------------

* Multiple atomic variables cannot form a single atomic transaction

  * Every single access is atomic, but the sequence of accesses is not
  * Preemption between consecutive reads produces inconsistent snapshots
  * Atomic ordering alone cannot tie unrelated memory locations together

.. code:: rust

  use core::sync::atomic::{AtomicU32, Ordering::Relaxed};

  // Weather station readings that belong together
  static TEMP: AtomicU32 = AtomicU32::new(10);
  static PRESSURE: AtomicU32 = AtomicU32::new(100);

  fn read_sample() -> (u32, u32) {
      // 1. Reads 10 
      let t = TEMP.load(Relaxed);

      // --- Preemption: ISR writes a new 'sample' (TEMP=25, PRESSURE=1015) ---

      // 2. Reads 1015 
      let p = PRESSURE.load(Relaxed);

      // Inconsistent result: 10 degrees paired with 1015 hPa
      (t, p)
  }

---------------------------------
Coherent Pair in an Atomic Word
---------------------------------

**Pack multiple values into one atomic integer**

  * Two 16-bit values into a single 32-bit atomic word
  * Hardware loads both values in one indivisible assembly instruction
  * Impossible for an interrupt to squeeze between the two reads

.. code:: rust

  use core::sync::atomic::{AtomicU32, Ordering::Relaxed};

  // Top 16 bits: Temperature | Bottom 16 bits: Pressure
  static SAMPLE: AtomicU32 = AtomicU32::new(0);

  pub fn publish(temperature: u16, pressure: u16) {
      let word = (u32::from(temperature) << 16) | u32::from(pressure);
      SAMPLE.store(word, Relaxed);
  }

  pub fn read() -> (u16, u16) {
      // Single 32-bit load reads both values simultaneously
      let word = SAMPLE.load(Relaxed);
      let temperature = (word >> 16) as u16;
      let pressure = word as u16;
      (temperature, pressure)
  }

.. note:: 

  Hard physical limit: cannot scale to large structs, arrays, or buffers

-------------------------------
Single-Core Critical Sections
-------------------------------

* Solution for complex data: temporarily disable (mask) interrupts

  * Hardware physically cannot preempt the foreground while interrupts are disabled
  * Foreground gains exclusive access to memory without data races
  * Satisfies the aliasing contract required by :rust:`UnsafeCell` to create :rust:`&mut T`

* Commonly implemented in embedded Rust via the :rust:`critical-section` crate

  * Interrupts are automatically re-enabled on exit
  * Paired with :rust:`RefCell` to provide safe, multi-field interior mutability

-----------------------------
Single-Core Deadlock Hazard
-----------------------------

* An interrupt handler must never spin waiting for the foreground loop

  * The interrupt preempts the foreground on the exact same CPU core
  * If the foreground holds a lock and the ISR spins waiting for it, system deadlocks
  * Foreground code can never run to release the lock while the ISR keeps spinning

.. code:: rust

  use core::sync::atomic::{AtomicBool, Ordering::Relaxed};

  static HELD: AtomicBool = AtomicBool::new(false);

  fn foreground_task() {
      HELD.store(true, Relaxed);
      // Preemption happens right here!
      HELD.store(false, Relaxed);
  }

  // FATAL BUG: Foreground cannot resume to clear 'HELD' while ISR spins
  fn on_interrupt() {
      while HELD.load(Relaxed) {
          // Deadlocks permanently on a single-core CPU
      }
  }
