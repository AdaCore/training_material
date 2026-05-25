=======
Panic
=======

---------------------
Unrecoverable Error
---------------------

* System errors are **not** recoverable

  * Failed bounds checks

    * Accessing :rust:`v[42]` on 3-element vector

  * Logic problems

    * Failed :rust:`assert!` or :rust:`debug_assert!`

* Call :rust:`panic!` when logic indicates unrecoverable error

  * Triggers thread shut-down
  * Configurable

    * Unwind stack and exit thread
    * Abort immediately

------------------
Panic Strategies
------------------

* **Stack Unwinding** (default)

  * Walk back up the stack and "clean up"

    * Runs :rust:`drop` for all objects in scope

  * Useful when working with hardware or multiple processes

    * Reset hardware flags
    * Release any locks
    * Exit thread

* **Aborting**

  * Instantly stops the program
  * Results in smaller binary size

    * No cleanup code

----------------------
Bounds Error Example
----------------------

.. code:: rust
  :number-lines: 1

  fn main() {
      let my_vector = vec![10, 20, 30];

      println!("{}", my_vector[42]);
  }

:error:`thread 'main' (32) panicked at src/main.rs:4:21:`

:error:`index out of bounds: the len is 3 but the index is 42`

----------------------
Manual Panic Example
----------------------

.. code:: rust
  :number-lines: 1

  fn main() {
      let my_vector = Vec::<i32>::new();

      if my_vector.is_empty() {
          panic!("Vector is empty!");
      }
  }

:error:`thread 'main' (12) panicked at src/main.rs:5:9:`

:error:`Vector is empty!`

----------------
When to Panic?
----------------

* **Prototyping**

  * :rust:`unwrap()` or :rust:`expect()` for quick coding

    * Replace with proper error handling later

* **Infallible Logic** (logic guarantees)

  * State that should never occur

* **Library Boundaries**

  * Libraries should return :rust:`Result`

    * Programmer decides how to handle the error

  * Panic if API **contract** is violated

    * E.g., passing an empty list to a function that requires at least one value
