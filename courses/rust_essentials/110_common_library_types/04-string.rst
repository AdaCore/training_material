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
  * :rust:`String can be borrowed as &str`
  * Functions often accept rust:`&str`

    * Implements :rust:`Deref<Target = str>`

.. note::

   :rust:`&str` is a fixed view into text, so not modifiable like :rust:`String`

------------------
Creating Strings
------------------

* Use :rust:`String::new()` when building text manually

  .. code:: rust

    let simple_string = String::new();

* Use traits/methods to create from literals

  .. code:: rust

    let string_to_str = String::from("hello");
    let str_to_string = "world".to_string();

-------------------
Modifying Strings
-------------------

* Strings are *mutable* (if declared :rust:`mut`) and support "append" methods

  .. code:: rust

    my_text.push('!');         // Append a char
    my_text.push_str(" foo");  // Append a &str

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

  .. raw:: latex

    \vspace{2mm}

  ``fancy_string.push_str("Greek letters:`` :math:`\lambda` :math:`\omega` :math:`\pi` ``");``

  ``println!("Length = {}", fancy_string.len());``

  ``println!("Chars = {}", fancy_string.chars().count());``

  .. raw:: latex

    \vspace{2mm}

:command:`Length = 23`

:command:`Chars = 20`

.. note::

  To get number of characters in :rust:`String` use :rust:`.chars().count()`
