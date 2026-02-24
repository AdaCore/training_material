========
Option
========

----------------------
What is "Option<T>"?
----------------------

:rust:`Option<T>` is defined as an enum with two variants

  * Like other user-defined types

  .. code:: rust

    enum Option<T> {
        Some(T),
        None,
    }

* :rust:`Some(T)`

  * Contains a value of type :rust:`T`

* :rust:`None`

  * Represents the absence of a value

----------------------
Why Use "Option<T>"?
----------------------

* Replaces the concept of *null* as an indication of no value

  * Forces user to check for "no value"

* Requires explicit handling before accessing the value

  * You must handle :rust:`None` before using :rust:`Some`

  * Eliminates null-pointer errors at compile time

* Is commonly used for

  * Optional arguments

  * Collection lookups

  * Functions that may not return a result

.. note::

  You cannot look at the value in :rust:`Some` without
  also checking for :rust:`None`

-------------------
Handling "Option"
-------------------

Code needs to handle either possiblity

* Most common handling

  * Indicates "no value" is not an unreasonable possibility

  .. code:: rust

    match opt {
      Some(next) => println!("{}", next),
      None       => println!("nothing found"),
    }

* Panic-based handling

  * When the code does not expect to encounter a missing value

    * These raise a panic exception

  .. code:: rust

    assert_eq!(position.unwrap(), 14);
    assert_eq!(position.expect("Character not found"), 0);

  * :rust:`unwrap` - return the value or panic
  * :rust:`expect` - panic with a message

----------------------
Benefits of "Option"
----------------------

* Safety and Clarity

  * No hidden *null* pointer 
  * Must explicitly handle absence of value

* Type system enforces handling of missing values

  * Cannot just ignore them
  * Fewer runtime surprises

.. note::

  Rust replaces *null* with :rust:`Option` so "nothing" can’t panic behind your back
