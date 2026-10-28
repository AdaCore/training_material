==========================
"Send" and "Sync" traits
==========================

-----------------
Standard Traits
-----------------

* :rust:`Send` and :rust:`Sync` contain no function definitions or runtime overhead

* Compiler automatically implements :rust:`Send` and :rust:`Sync` for your type

  * If all its constituent fields are :rust:`Send` and :rust:`Sync`

* :rust:`Send` allows moving ownership of :rust:`T` to another execution boundary

* :rust:`Sync` allows sharing :rust:`T` across boundaries 

  * Via shared immutable reference (:rust:`&T`)

* :rust:`T: Sync` if and only if :rust:`&T: Send`

--------
"Send"
--------

.. code:: rust

  use std::thread;

  struct MySendOnly {
    value: u32,
  }

  // Manually marking it as Send, but NOT Sync
  unsafe impl Send for MySendOnly {}

  fn main() {
    let data = MySendOnly { value: 10 };

    // ALLOWED: moving ownership to a new thread
    thread::spawn(move || {
        println!("Value moved to thread: {}", data.value);
    });

    // NOT ALLOWED: Sharing a reference across threads ('MySendOnly')
    // Attempting to pass 'data' to multiple threads would fail to compile!
  }


--------
"Sync"
--------

.. code:: rust

  use std::thread;

  struct MySyncOnly {
    value: u32,
  }

  // Manually marking it as Sync, but NOT Send
  unsafe impl Sync for MySyncOnly {}

  fn main() {
    let data = MySyncOnly { value: 20 };

    // ALLOWED: Multiple threads reading through a shared reference ('data')
    thread::scope(|s| {
        s.spawn(|| {
            println!("Thread 1 sees: {}", data.value); // Borrowed &data
        });
        s.spawn(|| {
            println!("Thread 2 sees: {}", data.value); // Borrowed &data
        });
    });

    // NOT ALLOWED: Moving ownership out of the thread!
    // `thread::spawn(move || data)` would fail to compile!
  }

-----------------
"Send" + "Sync"
-----------------

.. code:: rust

  use std::thread;

  struct MySendAndSync {
    value: u32,
  }

  // Implements both (this is what standard types get automatically)
  unsafe impl Send for MySendAndSync {}
  unsafe impl Sync for MySendAndSync {}

  fn main() {
    let data = MySendAndSync { value: 30 };

    // 1. You can borrow references across threads
    thread::scope(|s| {
        s.spawn(|| println!("Shared read: {}", data.value));
    });

    // 2. AND you can move total ownership to another thread afterward
    thread::spawn(move || {
        println!("Moved completely: {}", data.value);
    });
  }