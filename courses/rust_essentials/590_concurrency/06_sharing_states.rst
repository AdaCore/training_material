================
Sharing states
================

---------------------------------
The Interior Mutability Problem
---------------------------------

* Rust basic aliasing rule strictly forbids mutation via shared reference (&T)

* Real-world architectures frequently require modifying data through shared references

  * Graphs and cyclic structures
  * Shared state across OS threads
  * Global peripheral buffers in embedded systems

* Interior Mutability provides the escape hatch safely

  * Uses an internal :rust:`UnsafeCell<T>` to bypass compile-time aliasing rules
  * Moves borrow safety checks from compile time to runtime or hardware level

--------------------------
Single-Threaded: Cell<T>
--------------------------

* :rust:`Cell<T>` provides interior mutability by copying or moving values

  * Operates with zero runtime overhead or reference tracking
  * Never yields references to inner data (:rust:`&T` or :rust:`&mut T`)
  * Data must implement :rust:`Copy` for :rust:`.get()` or be replaced with :rust:`.set()`

.. code:: rust

  use std::cell::Cell;

  struct Widget {
      id: u32,
      render_count: Cell<u32>,
  }

  fn main() {
      // Widget is immutable, yet 'render_count' can be mutated
      let widget = Widget { id: 1, render_count: Cell::new(0) };

      widget.render_count.set(widget.render_count.get() + 1);
  }

-----------------------------
Single-Threaded: RefCell<T>
-----------------------------

* :rust:`RefCell<T>` allows dynamically borrowing values at runtime

  * Yields runtime-checked smart pointers (:rust:`Ref<T>` and :rust:`RefMut<T>`)
  * Tracks borrows using an internal non-atomic signed integer counter
  * Panics immediately at runtime if multiple borrows violate exclusive write rules

.. code:: rust

  use std::cell::RefCell;

  fn main() {
      let data = RefCell::new(vec![1, 2, 3]);

      // Dynamic mutable borrow
      data.borrow_mut().push(4);

      let r1 = data.borrow();
      // let mut r2 = data.borrow_mut(); 
	  // PANIC: Already borrowed immutably!
  }

----------------------------
The Bridge: Why They !Sync
----------------------------

* Single-threaded primitives are explicitly marked :rust:`!Sync`

  * Borrow counter inside :rust:`RefCell` is not atomic
  * Simultaneous read and write on 2 threads creates data race on counter
  * Compiler catches thread-crossing attempts at compile time

.. code:: rust

  use std::cell::RefCell;
  use std::thread;

  fn main() {
      let data = RefCell::new(10);

      // ERROR: `RefCell<i32>` cannot be shared safely between threads
      // thread::scope(|s| {
      //     s.spawn(|| {
      //         *data.borrow_mut() += 1;
      //     });
      // });
  }

-------------------------------
Interior Mutability Landscape
-------------------------------

.. list-table::
   :header-rows: 1
   :widths: 22 24 24 30
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
   * - :rust:`Mutex<T>`
     - OS thread blocking / futex
     - Kernel context switch
     - :rust:`Sync` (if :rust:`T: Send`)

--------------------------
Multi-Threaded: Mutex<T>
--------------------------

* :rust:`Mutex<T>` is the multi-threaded equivalent of RefCell

  * Guarantees mutual exclusion so only one thread accesses inner data at a time
  * Suspends and sleeps the requesting thread via OS futex when contested
  * Replaces runtime panics with graceful thread blocking

* RAII Guard pattern manages the critical section

  * Calling :rust:`.lock()` blocks the thread and returns a :rust:`MutexGuard`
  * Guard implements :rust:`DerefMut` to expose underlying :rust:`&mut T`
  * Dropping the guard releases the lock automatically

.. code:: rust

  use std::sync::Mutex;

  fn main() {
      let counter = Mutex::new(0);

      {
          // Blocks until the lock is acquired
          let mut guard = counter.lock().unwrap();
          *guard += 1;
          // Guard automatically drops here, unlocking the Mutex
      }

      assert_eq!(*counter.lock().unwrap(), 1);
  }

--------------------------
Pairing Mutex with Arc
--------------------------

* Mutex alone only handles thread synchronization, not shared ownership

  * Standard :rust:`std::thread::spawn` requires closures with a :rust:`'static` lifetime
  * Moving a Mutex into one thread relinquishes ownership from all other threads

* :rust:`Arc<Mutex<T>>` combines shared ownership with synchronized mutation

  * :rust:`Arc<T>` safely duplicates pointers and shares heap allocations
  * :rust:`Mutex<T>` locks and guards the actual data

.. code:: rust

  use std::sync::{Arc, Mutex};
  use std::thread;

  fn main() {
      let counter = Arc::new(Mutex::new(0));
      let mut handles = vec![];

      for _ in 0..4 {
          let counter_clone = Arc::clone(&counter);
          let handle = thread::spawn(move || {
              let mut guard = counter_clone.lock().unwrap();
              *guard += 1;
          });
          handles.push(handle);
      }

      for h in handles {
          h.join().unwrap();
      }

      println!("Total: {}", *counter.lock().unwrap());
  }

---------------------------------
Why Standard Mutex Fails in ISR
---------------------------------

* Hardware Interrupt Service Routines cannot use OS Mutex

  * Bare-metal systems lack an OS kernel, scheduler, and thread suspension
  * ISR preempts foreground execution on the exact same CPU core
  * If foreground main holds the lock and an ISR attempts to lock it, the ISR deadlocks forever

* Blocking is impossible in an interrupt handler

  * ISR must run quickly to completion without waiting
  * Traditional lock-and-wait semantics fundamentally break on bare metal

---------------------------------
Embedded: Critical Section Mutex
---------------------------------

* Embedded systems resolve shared mutation by masking interrupts

  * Disabling hardware interrupts guarantees no ISR can preempt main
  * Within interrupt-disabled window, memory access is safe from data race
  * With :rust:`RefCell` allows safe interior mutability in single-core :rust:`no_std`

.. code:: rust

  #![no_std]

  use core::cell::RefCell;
  use critical_section::Mutex;

  // Single-core interrupt-safe shared global container
  static SHARED_BUFFER: Mutex<RefCell<u32>> = Mutex::new(RefCell::new(0));

  fn main() {
      // Disable interrupts temporarily during execution of this closure
      critical_section::with(|cs| {
          let mut val = SHARED_BUFFER.borrow(cs).borrow_mut();
          *val += 10;
      });
  }

  #[no_mangle]
  pub fn TIMER0() {
      // ISR also acquires token 'cs' to ensure safety
      critical_section::with(|cs| {
          let mut val = SHARED_BUFFER.borrow_ref_mut(cs);
          *val = 0;
      });
  }
