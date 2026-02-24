========
Option
========

----------------------
Why Use "Option<T>"?
----------------------

* Replaces the concept of *null* as an indication of no value

  * Forces user to check for "no value"

* Requires explicit handling before accessing the value

  * You must handle :rust:`None` before using :rust:`Some`
  * Prevents null dereference in safe code

.. note::

  You cannot look at the value in :rust:`Some` without
  also checking for :rust:`None`

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

  .. code:: rust

    assert_eq!(position.unwrap(), 14);
    assert_eq!(position.expect("Character not found"), 0);

  * :rust:`unwrap` - return the value or panic
  * :rust:`expect` - panic with a message

  * Useful for prototypes, tests, guaranteed-to-pass invariants

    * Should **not** be used in production code

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

------------------
Common Use Cases
------------------

* Optional arguments

  * Configuration data wherer a value might not be specified

  .. code:: rust

    struct Config {
        output_file: Option<String>,
    }

* Collection lookups

  * Looking for something in a database that might not be there

  .. code:: rust

    fn find_user(id: u32) -> Option<User> {
        // returns None if user doesn’t exist
    }

* Functions that may not return a result

  * Popping from an empty stack

  .. code:: rust

    let value = stack.pop();

