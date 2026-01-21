=========
Methods
=========

-----------------
Methods in Rust
-----------------

* What is a :dfn:`method?`

  * Function *associated* with type via :rust:`impl` block

  * Syntax: :rust:`instance.method(...)`

  * First parameter :dfn:`(receiver)` determines how the method uses the value

* Why use methods?

  * Organize behavior with the data it operates on

  * Makes code more ergonomic and readable

------------------
Method Receivers
------------------

.. list-table::
  :header-rows: 1

  * - Receiver
    - Meaning

  * - :rust:`&self`
    - Shared, read-only borrow

  * - :rust:`&mut self`
    - Unique, mutable borrow

  * - :rust:`self`
    - Takes ownership

  * - :rust:`mut self`
    - Takes and mutably uses ownership

  * - *No receiver*
    - Associated function

---------------------------------
Method Receiver - Shared Borrow
---------------------------------

* Definition

  .. code:: rust

    struct Counter {
        value: i32,
    }

    impl Counter {
        fn get(&self) -> i32 {
            self.value
        }
    }

* Usage

  .. code:: rust

    let c = Counter { value: 5 };
    let a = c.get();

    // OK: multiple shared borrows
    let b = c.get();

* Behavior

  * Read-only access
  * Value remains usable after calls

---------------------------------
Method Receiver - Mutable Borrow
---------------------------------

* Definition

  .. code:: rust

    impl Counter {
        fn increment(&mut self) {
            self.value += 1;
        }
    }

* Usage

  .. code:: rust

    let mut c = Counter { value: 0 };
    c.increment();

    // OK, sequential mutable borrows
    c.increment();

    let bad_c = Counter { value: 0 };
    // error: cannot borrow immutable value as mutable
    bad_c.increment();

* Behavior

  * Exclusive access
  * Caller must declare the value :rust:`mut`

----------------------------------
Method Receiver - Take Ownership
----------------------------------

* Definition

  .. code:: rust

    impl Counter {
        fn finish(self) -> i32 {
            self.value
        }
    }

* Usage

  .. code:: rust

    let c = Counter { value: 10 };
    let v = c.finish();

    // Error: use of moved value
    c.get();

* Behavior

  * Value is moved into the method
  * Object cannot be reused afterward

-------------------------------------
Method Receiver - Mutable Ownership
-------------------------------------

* Definition

  .. code:: rust

    impl Counter {
        fn reset(mut self) -> Self {
            self.value = 0;
            self
        }
    }

* Usage

  .. code:: rust

    let c = Counter { value: 5 };

    // ownership moved, new value returned
    let c = c.reset();

    // Error if you don't rebind: value was moved
    c.reset();

* Behavior

  * Takes ownership and allows mutation
  * Original value is consumed
  * Typically used in builder / chaining APIs

-------------------------------
Method Receiver - No Receiver
-------------------------------

* Also called :dfn:`associated receiver`

* Definition

  .. code:: rust

    impl Counter {
        fn new() -> Self {
            Counter { value: 0 }
        }
    }

* Usage

  .. code:: rust

    // call on the type, not an instance
    let c = Counter::new();

    // OK: c is now a normal value
    println!("{}", c.value);

* Behavior

  * Not called on an instance
  * No access to existing fields (self is unavailable)
