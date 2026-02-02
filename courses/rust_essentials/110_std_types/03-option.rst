========
Option
========

----------------------
What Is "Option<T>"?
----------------------

* :rust:`Option<T>` represents an optional value in Rust:

  * :rust:`Some(T)`

    * contains a value of type T

  * :rust:`None`

    * Represents no value

* Rust uses :rust:`Option` instead of nullable pointers (:rust:`null`)

  * Force handling of absence explicitly

.. code:: rust

  let pos: Option<usize> = name.find('+');
  // "pos" will be a position (of "usize") or "None"

-------------------
Handling "Option"
-------------------

* You need to handle either possiblity

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

  * No hidden :rust:`null` pointer 
  * Must explicitly handle absence of value

* Type system makes you think about missing values

  * Cannot just ignore them
  * Fewer runtime surprises
  

