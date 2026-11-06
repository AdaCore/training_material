================
Sharing States
================

----------------------------
Need for Global Visibility
----------------------------

* Interrupt handlers take zero arguments and return nothing

  * Triggered asynchronously by hardware not called by program logic
  * Handlers cannot receive local stack variables from foreground callers

* Data shared across execution contexts must reside in static storage

  * Static variables have a :rust:`'static` lifetime valid for entire runtime
  * Plain statics are immutable by default

-------------------------------
Issue of "static mut" in Rust
-------------------------------

* Global mutable statics bypass ownership and create aliasing disasters

  * Reading or writing a :rust:`static mut` requires an :rust:`unsafe` block
  * Produces a hard compiler error since Rust 2024
  * Raw pointer writes provide no synchronization against interrupt preemption

.. code:: rust

  // DANGER: Conflicting access without synchronization
  static mut COUNT: u32 = 0;

  fn foreground_step() {
      unsafe {
          // COMPILE ERROR in Rust 2024: references to mutable statics disallowed
          COUNT += 1; 
      }
  }

  fn on_interrupt() {
      unsafe {
          COUNT = 0;
      }
  }

-----------------
Standard Traits
-----------------

* :rust:`Send` allows moving ownership of :rust:`T` to another execution boundary
* :rust:`Sync` allows sharing :rust:`T` across boundaries 

  * Via shared immutable reference (:rust:`&T`)

* :rust:`T: Sync` if and only if :rust:`&T: Send`
* :rust:`Send` and :rust:`Sync` contain no function definitions or runtime overhead
* Compiler automatically implements :rust:`Send` and :rust:`Sync` for your type

  * If all its constituent fields are :rust:`Send` and :rust:`Sync`

-----------------------------------
Why Cell and RefCell Are Not Sync
-----------------------------------

* Single-threaded interior mutability moves borrow checks to runtime

  * :rust:`Cell<T>` allows mutating values by copying or moving data in place
  * :rust:`RefCell<T>` dynamically tracks active borrows using an internal counter

* Both types are explicitly marked :rust:`!Sync`

  * Borrow tracking counters are standard non-atomic integers
  * Interleaved updates to borrow counters create immediate data races
  * Compiler refuses to place :rust:`Cell` or :rust:`RefCell` into a shared :rust:`static`

.. code:: rust

  use core::cell::Cell;

  // The trait `Sync` is not implemented for `Cell<u32>`
  // ERROR
  // static BAD: Cell<u32> = Cell::new(0);
  
.. container:: latex_environment footnotesize
  
  :error:`error[E0277]: `Cell<u32>` cannot be shared between threads safely`

-----------------------------------
"UnsafeCell": The Ultimate Escape
-----------------------------------

* :rust:`UnsafeCell<T>` is the core language primitive for interior mutability

  * Mutates memory behind a shared reference :rust:`&T`
  * Compiler assume data behind :rust:`&UnsafeCell<T>` is not constant

.. code:: rust

  use core::cell::UnsafeCell;

  // Caller must guarantee exclusive access across all competing contexts
  unsafe fn increment_exclusive(cell: &UnsafeCell<u32>) {
      // Calling .get() is completely safe: returns *mut u32
      let pointer = cell.get();

      // SAFETY: Caller upholds the contract that no ISR can interrupt this write
      unsafe {
          *pointer = (*pointer).wrapping_add(1);
      }
  }

* Calling :rust:`cell.get()` is safe, but dereferencing raw pointer requires :rust:`unsafe`

-------------------------------
The Exclusive-Access Contract
-------------------------------

* :rust:`UnsafeCell<T>` does not provide any synchronization by itself

  * Does not disable interrupts, mask peripherals, or block execution
  * Explicitly marked :rust:`!Sync` by the compiler

* The abstraction bridge in bare-metal systems

  * Target-specific platform backend disables or masks interrupts
  * Safe embedded wrappers safely expose :rust:`&mut T` to user application code

* Borrow must never escape protected scope or survive interrupt unmasking


