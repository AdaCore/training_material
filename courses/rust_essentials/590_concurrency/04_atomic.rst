========
Atomic
========

--------------------------
Atomics for Safe Sharing
--------------------------

* :rust:`atomic` types resolve shared-state issues without locks or :rust:`static mut`

  * Part of the standard :rust:`core::sync::atomic` module
  * Modifies values via shared immutable references (:rust:`&self`)
  * Implement the :rust:`Sync` marker trait so they can live in a :rust:`static`

* Guaranteed indivisible at the CPU instruction level

  * Requires no operating system scheduler
  * Preemption by an interrupt cannot observe or cause a half-written state

-------------------------------
Interior Mutability Landscape
-------------------------------

.. list-table::
   :header-rows: 1

   * - **Mechanism**
     - **How it works**
     - **Execution Cost**
     - **Sync**
   * - :rust:`Cell<T>`
     - Value copies and moves
     - Zero overhead
     - :rust:`!Sync`
   * - :rust:`RefCell<T>`
     - Dynamic borrow counter
     - Minor branch checking
     - :rust:`!Sync`
   * - :rust:`Atomic`
     - CPU atomic instructions
     - Lock-free assembly
     - :rust:`Sync`
  
----------------
"atomic" Types 
----------------

* :rust:`AtomicBool`

  * *Construction:* 1-byte thread-safe boolean
  * *Used for:* Hardware signaling, event pending flags
  
* :rust:`AtomicUsize` / :rust:`AtomicIsize`

  * *Construction:* Pointer-sized integer
  * *Used for:* Buffer indices, ring buffer pointers
  
* :rust:`AtomicU32` / :rust:`AtomicI32`

  * *Construction:* 32-bit primitive
  * *Used for:* Event counters, metrics, packed words
  
* :rust:`AtomicPtr<T>`

  * *Construction:* Raw pointer (*:rust:`mut T`)
  * *Used for:* Lock-free node links, buffer handoffs

---------------------
"atomic" Operations
---------------------

.. list-table::
   :header-rows: 1

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

  * Constrains compiler and CPU to reorder surrounding memory accesses
  * Select ordering based on what the atomic variable is communicating

* **Relaxed**

  * Guarantees atomicity for that specific memory word only
  * No synchronization or ordering guarantees across other variables
  * Correct choice when the atomic variable itself is the entire message

- **Release (Store)**

  * Prevents prior reads and writes from being reordered after this store
  * Publishes prior changes to contexts doing a matching :rust:`Acquire` load


* **Acquire (Load)**

  * Memory reads observe changes made prior to matching :rust:`Release`
  * Used by a consumer to acquire published data safely

* **AcqRel**

  * Combines :rust:`Acquire` and :rust:`Release`
  
    * For read-modify-write operations (:rust:`.swap()`)

* **SeqCst**

  * Enforces a globally consistent order across all cores
  * Incurs extra memory barrier overhead on many architectures
  

-----------------------------
Target Hardware Limitations
-----------------------------

* Not every embedded microcontroller supports all atomic types

  * Limited by architecture instruction set
  * Smaller microcontrollers may lack 32-bit atomic instructions

* The compiler guards architecture capabilities via configuration flags

  * 8-bit support
  
    * :rust:`#[cfg(target_has_atomic = "8")]`
	
  * 32-bit support

    * :rust:`#[cfg(target_has_atomic = "32")]`

--------------------------
Atomic Read-Modify-Write
--------------------------

* Separate loads and stores reintroduce interleaving hazards

  * Loading an atomic and storing later are two distinct operations
  * An interrupt firing between them can erase notifications

.. code:: rust

  use core::sync::atomic::{AtomicI32, Ordering::Relaxed};

  static COUNTER: AtomicI32 = AtomicI32::new(0);

  // WRONG: Split load and store can overwrite another context's update!
  fn increment_wrong() {
      let current = COUNTER.load(Relaxed);
      // PREEMPTION HAZARD: An ISR could modify COUNTER right here!
      COUNTER.store(current + 1, Relaxed);
  }

  // CORRECT: Indivisible read-modify-write hardware instruction
  fn increment_correct() {
      COUNTER.fetch_add(1, Relaxed);
  }

--------------------------------
Event Flags vs. Event Counters
--------------------------------

* Choose the atomic primitive based on communication requirements

* **Event Flags:** :rust:`AtomicBool`

  * Answers: "Did at least one event happen?"
  * Multiple rapid interrupts coalesce into a single notification flag
  * Ideal for work-needed notifications and wake-up triggers

* **Event Counters:** :rust:`AtomicU32`

  * Answers: "How many events occurred?"
  * Uses :rust:`.fetch_add(1, Relaxed)` so no occurrences are lost
  * Foreground uses :rust:`.swap(0, Relaxed)` to consume total batch count

-------------------------------
One-Time Publication Protocol
-------------------------------

* Use :rust:`Acquire` and :rust:`Release` to synchronize access to other state

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

* Volatile flags external access such as hardwares sensors

* :rust:`read_volatile` **And** :rust:`write_volatile`

  * Ensure compiler does not optimize away memory access operations
  * Guarantee an access occurs
  
    * But provide zero cross-context synchronization
	
  * Do not prevent preemption or instruction reordering across contexts

* **Atomic Operations**

  * Specifically for sharing and mutating states safely across contexts
  * **Indivisible** updates preventing data races
  
    * And torn values under preemption
  
-----------------------------
When Atomics Are Not Enough
-----------------------------

**Multiple atomic variables cannot form a single atomic transaction**

  * Every single access is atomic, but the sequence of accesses is not
  * Preemption between consecutive reads produces inconsistent snapshots
  * Atomic ordering alone cannot tie unrelated memory locations together

.. code:: rust

  use core::sync::atomic::{AtomicU32, Ordering::Relaxed};

  // Weather station readings that belong together
  static TEMP: AtomicU32 = AtomicU32::new(10);
  static PRESSURE: AtomicU32 = AtomicU32::new(100);

  fn read_sample() -> (u32, u32) {
      // Reads 10 
      let t = TEMP.load(Relaxed);

      // --- Preemption: ISR writes a new 'sample' (TEMP=25, PRESSURE=1015) ---

      // Reads 1015 
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
The Exclusive-Access Contract
-------------------------------

* :rust:`UnsafeCell<T>` does not provide any synchronization by itself

  * Does not disable interrupts, mask peripherals, or block execution
  * Explicitly marked :rust:`!Sync` by the compiler

* Application converts hardware guarantees into safe Rust

  * Target-specific platform backend disables or masks interrupts
  * Safe wrappers expose :rust:`&mut T` to user application code

* Borrow must never 

  * Escape protected scope
  * Continue to exist after interrupts are enabled again


------------------------------------
Critical Sections for Complex Data
------------------------------------

* Critical sections protect shared data from concurrent access

  * Temporarily disable relevant interrupts
  * Allow foreground code to access the data without ISR interference
  * Satisfies the aliasing contract required by :rust:`UnsafeCell` to create :rust:`&mut T`

* Commonly implemented via a :rust:`critical-section`

  * Interrupts are automatically re-enabled on exit
  * Paired with :rust:`RefCell` to provide safe, multi-field interior mutability
  
.. note:: 

  :rust:`critical-section` is an external crate not part of :rust:`core`

-----------------------------
Single-Core Deadlock Hazard
-----------------------------

* On a single-core system, foreground cannot resume until ISR returns

  * Interrupts preempt the foreground on the exact same CPU core
  * If foreground holds a lock and ISR spins waiting, system deadlocks
  * Foreground code can never release the lock while ISR keeps spinning

.. code:: rust

  use core::sync::atomic::{AtomicBool, Ordering::Relaxed};

  static LOCK: AtomicBool = AtomicBool::new(false);

  fn foreground_task() {
      LOCK.store(true, Relaxed);
      // Preemption 'on_interrupt()' is called 
      LOCK.store(false, Relaxed);
  }

  fn on_interrupt() {
    // DANGER: Cannot load because lock is held 
    // Cannot be released because no more interrupts to clear the lock
    while LOCK.load(Relaxed) {
        // Deadlocks permanently on a single-core CPU
    }
  }
