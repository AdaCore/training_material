================
Sharing States
================

----------------------------
Need for Global Visibility
----------------------------

* Interrupt handlers take zero arguments and return nothing

  * Triggered asynchronously by hardware

    * Not called by program logic

  * Handlers cannot access local stack variables from the foreground

* :rust:`'static` items provide storage accessible to both execution contexts

  * Exist for the program's lifetime
  * :rust:`static` variables are immutable by default
  * But ISR can interrupt a read/write

-----------------
Standard Traits
-----------------

* :rust:`Send` permits moving ownership of :rust:`T` to another execution boundary
* :rust:`Sync` permits sharing :rust:`T` across boundaries 

  * Via shared immutable reference (:rust:`&T`)

* Type :rust:`T` is safe to share across contexts (:rust:`Sync`) via an immutable reference (:rust:`&T`)

  * Only if that reference is also safe to transfer (:rust:`Send`)

* :rust:`Send` and :rust:`Sync` contain no function definitions or runtime overhead
* Compiler **automatically** implements :rust:`Send` or :rust:`Sync` for your type

  * If all its constituent fields are :rust:`Send` or :rust:`Sync`

---------------------------------------
Why "Cell" and "RefCell" Are Not Sync
---------------------------------------

* :rust:`Cell<T>` allows values to be replaced through shared references 

  * It does not perform runtime borrow checking

* :rust:`RefCell<T>` dynamically tracks active borrows using an internal counter

* Both types are explicitly marked :rust:`!Sync`

  * :rust:`RefCell` borrow tracking counters are standard non-atomic integers
  * Concurrent writes/reads cause unsynchronized memory mutation
  * Compiler refuses to place :rust:`Cell` or :rust:`RefCell` into a shared :rust:`static`

.. code:: rust

  use core::cell::Cell;

  // The trait 'Sync' is not implemented for 'Cell<u32>'
  static BAD: Cell<u32> = Cell::new(0);
  
.. code:: error
  :font-size: footnotesize

  error[E0277]: `Cell<u32>` cannot be shared between threads safely

-----------------------------------
"UnsafeCell": The Ultimate Escape
-----------------------------------

* :rust:`UnsafeCell<T>` is the core language primitive for interior mutability

  * Mutates memory behind a shared reference :rust:`&T`
  * Compiler assumes data behind :rust:`&UnsafeCell<T>` is not constant

.. code:: rust

  use core::cell::UnsafeCell;

  // Caller must guarantee exclusive access across all competing contexts
  unsafe fn increment_exclusive(cell: &UnsafeCell<u32>) {
      // Calling '.get()' is completely safe: returns '*mut u32'
      let pointer = cell.get();

      // SAFETY: Caller upholds the contract that no ISR can interrupt this write
      unsafe {
          *pointer = (*pointer).wrapping_add(1);
      }
  }

* Calling :rust:`cell.get()` is safe

  * Dereferencing raw pointer requires :rust:`unsafe`



