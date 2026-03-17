=========
Methods
=========

-----------------
Methods in Rust
-----------------

* What is a :dfn:`method`?

  * Function *associated* with type via :rust:`impl` block

* Why use methods?

  * Organize behavior with the data it operates on


.. container:: latex_environment tiny

  .. code:: rust
    :number-lines: 3

    struct CarRace {
        laps: Vec<i32>,
    }

    impl CarRace {
        // Method: Modify data
        fn record_lap(&mut self, time: i32) {
            self.laps.push(time);
        }
    }

    fn main() {
        let mut race = CarRace::new();
        race.record_lap(114); // Data and logic live together
    }

----------------------------
What is a Method Receiver?
----------------------------

* First parameter of a **method**

  * Named :rust:`self`

* Specifies how the method accesses the value it is called on

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

    struct Counter {
        value: i32,
    }

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

.. code:: rust

  impl Counter {
      // This 'consumes' counter and returns the final number
      fn finalize(self) -> i32 {
          println!("Shutting down counter...");
          // Return the value, 'self' is dropped here
          self.value
      }
  }

  let count = Counter { value: 10 };
  let total = count.finalize();

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

    struct Counter {
        value: i32,
    }

    impl Counter {
        fn reset(mut self) -> Self {
            self.value = 0;
            self
        }
    }

**Usage**

  .. code:: rust

    let count = Counter { value: 5 };

    // Ownership moved, new value returned
    let count = count.reset();

    count.reset();
    // Because we do not capture the return value
    // It is moved to nowhere - 'count' is no longer the owner

**Behavior**

  * Takes ownership and allows mutation
  * Original value is consumed
  * :dfn:`Builder pattern`

    * Allows construction of complex objects
    * Chain multiple calls together

-------------------------------
Method Receiver - No Receiver
-------------------------------

**Definition**

  .. code:: rust

    struct Counter {
        value: i32,
    }

    impl Counter {
        fn new() -> Self {
            Counter { value: 0 }
        }
    }

**Usage**

  .. code:: rust

    // Call on the type, not an instance
    let count = Counter::new();

    // OK: count is now a normal value
    println!("{}", count.value);

**Behavior**

  * Not called on an instance
  * No access to existing fields (:rust:`self` is unavailable)

.. note::

  This is usually called an :dfn:`associated function`

------------------------------
Method Receivers at a Glance
------------------------------

* :rust:`&self` (**The Reader** - *shared, read-only borrow*)

  * *Power*: Can read data but cannot change it
  * *Usage*: Multiple parts of the code can call this at the same time

.. raw:: latex

   \vspace{1mm}

* :rust:`&mut self` (**The Writer** - *unique, mutable borrow*)

  * *Power*: Can modify the internal data of the object
  * *Usage*: Exclusive access; no one else has visibility while running

.. raw:: latex

   \vspace{1mm}

* :rust:`self` (**The Owner** - *takes full ownership*)

  * *Power*: Can destroy, move, or transform the object
  * *Usage*: Object is "consumed" - cannot be used again after the call

.. raw:: latex

   \vspace{1mm}

* :rust:`mut self` (**The Builder** - *takes ownership and allows mutation*)

  * *Power*: You can change an object you are about to return or discard
  * *Usage*: Common "Builder Pattern" for constructing complex objects

.. raw:: latex

   \vspace{1mm}

* No Receiver (**The Helper** - *associated function*)

  * *Power*: No access to a specific instance or its fields
  * *Usage*: Usually used for constructors, like :rust:`Counter::new()`
