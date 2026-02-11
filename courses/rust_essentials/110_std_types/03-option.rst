========
Option
========

--------------------------------
"Option<T>" - Handling Absence
--------------------------------

* Use :rust:`Option<T>` to detect missing values

  * Instead of using *null* as other languages
  * Forces you to acknowledge when a value might be missing

* Defined as an enum with two variants

  * :rust:`Some(T)`

    * Contains a value of type T

  * :rust:`None`

    * Represents the absence of a value

* Cannot look at value in :rust:`Some` unless you check for :rust:`None`

  * Eliminates "Null Pointer Exceptions" at compile time

* Used for

  * Optional arguments
  * Lookups in a collection (like a :rust:`HashMap`)
  * Any function that might fail to return a result

-------------------
Handling "Option"
-------------------

* You must handle either possiblity

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
  

