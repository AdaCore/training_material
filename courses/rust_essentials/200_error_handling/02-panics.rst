========
Panics
========

---------------------
Unrecoverable Error
---------------------

* :rust:`panic` is :dfn:`Rust's way of handling a fatal runtime error`

  * Immediately stops execution of current thread

* When it happens:

  * *Bugs* - failed bounds checks

    * Example: accessing :rust:`v[100]` on a 3-item vector

  * *Logic failures* - failed :rust:`assert!` or :rust:`debug_assert!`

  * *Manual trigger* - :rust:`panic!("message")` macro

    * Typically when program reaches an impossible state

.. tip::

  Panics are for unexpected/unrecoverable errors, whereas :rust:`Result` is for expected errors

-----------------------------------
Mechanics - Unwinding vs Aborting
-----------------------------------

* Stack unwinding (default)

  * Rust walks back up the stack and "cleans up"

    * Calls destructors (:rust:`drop`) for all objects in scope

  * Ensures resources released even during a crash

    * Like memory or file handles

* Aborting

  * Can configure Rust to *abort* on panic
  * Stops the program instantly without cleanup

    * Results in smaller binary size

* Safe Alternatives

  * :rust:`v[i]` panics on out-of-bounds
  * Instead, use :rust:`v.get(i)` to return an :rust:`Option`

--------------
Code Example
--------------

* Bounds error

  .. code:: rust
    :number-lines: 1

    fn main() {
        let v = vec![10, 20, 30];
    
        println!("{}", v[100]); 
    }

  :error:`thread 'main' (32) panicked at src/main.rs:4:21:`

  :error:`index out of bounds: the len is 3 but the index is 100`

* Manual panic

  .. code:: rust
    :number-lines: 1

    fn main() {
        let v = vec![10, 20, 30];

        if v.len() < 10 {
            panic!("Vector is too short!");
        }
    }

  :error:`thread 'main' (12) panicked at src/main.rs:5:9:`

  :error:`Vector is too short!`

----------------
When to Panic?
----------------

* Examples and prototypes

  * Use :rust:`unwrap()` or :rust:`expect()` when writing quick code

    * You don't care about error handling yet

* Infallible logic (logic guarantees can never happen)

  * Panic is appropriate - indicates a bug

* Library Boundaries

  * Libraries should return :rust:`Result`

    * User decides how to handle the error

  * Panic if user violates the **contract** of the API

    * E.g., passing an empty list to a function that requires items
