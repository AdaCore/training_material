========
String
========

-------------------
What Is "String"?
-------------------

* :rust:`String` is an **owned** and **growable** UTF-8 encoded type

  * Lives on the heap
  * Grows as needed
  * Ensures valid UTF-8

* Key points

  * UTF-8 means length in bytes not necessarily number of characters
  * Implements :rust:`Deref<Target = str>`

    * converts to a string slice (:rust:`&str`) when passed to functions

------------------
Creating Strings
------------------

* Use :rust:`String::new()` when building text manually

  .. code:: rust

    let s1 = String::new();

* Use traits/methods to create from literals

  .. code:: rust

    let s2 = String::from("hello");
    let s3 = "world".to_string();

-------------------
Modifying Strings
-------------------

* Strings are *mutable* (if declared :rust:`mut`) and support "append" methods

  .. code:: rust

    s.push('!');         // append a char
    s.push_str(" foo");  // append a &str

.. note::

  Because :rust:`String` owns its buffer, it may reallocate as it grows

----------------------
Length vs Characters
----------------------

* :rust:`::chars()`

  * Iterator over actual characters

    * Characters are UTF-8, so may be multiple bytes

* :rust:`.len()`

  * Size of string in bytes (**not** characters)

.. code:: rust

    let mut s1 = String::new();
    s1.push_str("Hello");
    println!(
        "s1: len = {}, number of chars = {}",
        s1.len(),
        s1.chars().count()
    );

:command:`s1: len = 5, number of chars = 5`

.. note::

  To get number of characters in :rust:`String` use :rust:`.chars().count()`
