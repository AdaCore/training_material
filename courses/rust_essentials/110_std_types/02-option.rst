========
Option
========

-------------
"Option<T>"
-------------

:rust:`Option<T>` is defined as an enum with two variants

* :rust:`Some(T)`

  * Contains a value of type :rust:`T`

* :rust:`None`

  * Represents the absence of a value

--------------------------------
"Option<T>" - Handling Absence
--------------------------------

:rust:`Option<T>`

* Represents a value that may be missing

  * Used instead of null in other languages

  * Forces you to acknowledge when a value might be absent

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

Handle either possiblity

* Typical handling

  .. code:: rust

    match opt {
      Some(v) => println!("{}", v),
      None    => println!("no value"),
    }

* Panic-based handling

  .. code:: rust

    assert_eq!(position.unwrap(), 14);
    assert_eq!(position.expect("Character not found"), 0);

  * :rust:`unwrap` - return the value or panic
  * :rust:`expect` - panic with a message

-------------------
Why Use "Option"?
-------------------

* Safety and Clarity

  * No hidden *null* pointer 
  * Must explicitly handle absence of value

* Type system enforces handling of missing values

  * Cannot just ignore them
  * Fewer runtime surprises

.. note::

Rust replaces *null* with :rust:`Option` so “nothing” can’t panic behind your back
