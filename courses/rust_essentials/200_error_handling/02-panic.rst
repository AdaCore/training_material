=======
Panic
=======

---------------------
Unrecoverable Error
---------------------

* Runtime errors are not recoverable

  * Failed bounds checks

    * Accessing :rust:`v[100]` on a 3-item vector

  * Logic problems

    * Failed :rust:`assert!` or :rust:`debug_assert!`

* Call :rust:`panic!` when logic indicates unrecoverable error

  * Triggers thread shut-down
  * Configurable - either unwind stack or abort

------------------------------
What Happens During a Panic?
------------------------------

* **Stack unwinding** (default)

  * Rust walks back up the stack and "cleans up"

    * Runs :rust:`drop` for all objects in scope

  * Useful when working with hardware or multiple processes

    * Reset hardware flags
    * Release any locks

* **Aborting** (configurable)

  * Instantly stops the program
  * Results in smaller binary size

    * No cleanup code

--------------
Code Example
--------------

**Bounds error**

  .. code:: rust
    :number-lines: 1

    fn main() {
        let my_vector = vec![10, 20, 30];
    
        println!("{}", my_vector[100]); 
    }

  :error:`thread 'main' (32) panicked at src/main.rs:4:21:`

  :error:`index out of bounds: the len is 3 but the index is 100`

**Manual panic**

  .. code:: rust
    :number-lines: 1

    fn main() {
        let my_vector = vec![10, 20, 30];

        if my_vector.len() < 10 {
            panic!("Vector is too short!");
        }
    }

  :error:`thread 'main' (12) panicked at src/main.rs:5:9:`

  :error:`Vector is too short!`

----------------
When to Panic?
----------------

* **Prototyping**

  * :rust:`unwrap()` or :rust:`expect()` for quick coding 

    * Replace with proper error handling later

* **Infallible logic** (logic guarantees)

  * In a state that should never occur
  * Panic indicates a bug

* **Library boundaries**

  * Libraries should return :rust:`Result`

    * Programmer decides how to handle the error

  * Panic if API **contract** is violated

    * E.g., passing an empty list to a function that requires items
