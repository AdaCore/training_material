=========
Methods
=========

-----------------
Methods in Rust
-----------------

**Example**

  .. code:: rust

    struct CarRace {
        name: String,
        laps: Vec<i32>,
    }
    impl CarRace {
        fn method(&mut self, lap: i32) {
            self.laps.push(lap);
        }
    let mut instance = CarRace::method("Monaco Grand Prix");

* What is a :dfn:`method`?

  * Function *associated* with type via :rust:`impl` block

  * First parameter :dfn:`(receiver)` determines how the method uses the value

* Why use methods?

  * Organize behavior with the data it operates on

  * Makes code more ergonomic and readable

----------------------------
What is a Method Receiver?
----------------------------

* First parameter of a method

  * Named :rust:`self`

* Tells Rust how the method gets access to self

.. code:: rust

    fn method(&mut self, lap: i32) {
        self.laps.push(lap);
    }

* Determines whether method

  * Takes ownership
  * Borrows immutably
  * Borrows mutably

---------------------------------
Method Receiver - Shared Borrow
---------------------------------

**Definition**

  .. code:: rust

    struct Counter {
        value: i32,
    }

    impl Counter {
        fn get(&self) -> i32 {
            self.value
        }
    }

**Usage**

  .. code:: rust

    let count = Counter { value: 5 };
    let val1 = count.get();

    // OK: multiple shared borrows
    let val2 = count.get();

**Behavior**

  * Read-only access
  * Value remains usable after calls

---------------------------------
Method Receiver - Mutable Borrow
---------------------------------

**Definition**

  .. code:: rust

    impl Counter {
        fn increment(&mut self) {
            self.value += 1;
        }
    }

**Usage**

  .. code:: rust

    let mut count = Counter { value: 0 };
    count.increment();

    // OK, sequential mutable borrows
    count.increment();

    let bad = Counter { value: 0 };
    bad.increment();

:error:`error[E0596]: cannot borrow "bad" as mutable, as it is not declared as mutable`

**Behavior**

  * Exclusive access
  * Caller must declare the value :rust:`mut`

----------------------------------
Method Receiver - Take Ownership
----------------------------------

**Definition**

  .. code:: rust

    impl Counter {
        fn finish(self) -> i32 {
            self.value
        }
    }

**Usage**

  .. code:: rust

    let count = Counter { value: 10 };
    let new_count = count.finish();

    count.get();

:error:`error[E0382]: borrow of moved value: "count"`

**Behavior**

  * Value is moved into the method
  * Object cannot be reused afterward

-------------------------------------
Method Receiver - Mutable Ownership
-------------------------------------

**Definition**

  .. code:: rust

    impl Counter {
        fn reset(mut self) -> Self {
            self.value = 0;
            self
        }
    }

**Usage**

  .. code:: rust

    let count = Counter { value: 5 };

    // ownership moved, new value returned
    let count = count.reset();

    count.reset();
    // Because we do not capture the return value
    // it is moved to nowhere - "count" is no longer the owner

**Behavior**

  * Takes ownership and allows mutation
  * Original value is consumed
  * :dfn:`Builder pattern`

    * Allows construction of complex objects
    * Chain multiple calls together

-------------------------------
Method Receiver - No Receiver
-------------------------------

* Also called :dfn:`associated function`

**Definition**

  .. code:: rust

    impl Counter {
        fn new() -> Self {
            Counter { value: 0 }
        }
    }

**Usage**

  .. code:: rust

    // call on the type, not an instance
    let count = Counter::new();

    // OK: count is now a normal value
    println!("{}", count.value);

**Behavior**

  * Not called on an instance
  * No access to existing fields (:rust:`self` is unavailable)

----------------------------
Method Receivers - Summary
----------------------------

.. list-table::
  :header-rows: 1

  * - **Receiver**
    - **Meaning**

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

