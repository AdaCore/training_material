========
String
========

-------------------
What Is "String"?
-------------------

:rust:`String` **is an owned and growable UTF-8 encoded type**

* Lives on the heap
* Grows as needed
* Ensures valid UTF-8

  * Length in bytes not necessarily number of characters

.. note::

  * Functions often accept :rust:`&str`
  * :rust:`&str` is a fixed view into text, so not modifiable like :rust:`String`

------------------
Creating Strings
------------------

* :rust:`String::new()` builds text manually

  .. code:: rust

    let simple_string = String::new();

* Traits/methods create from literals

  .. code:: rust

    let string_to_str = String::from("hello");
    let str_to_string = "world".to_string();

-------------------
Modifying Strings
-------------------

**Strings support "append" methods (if mutable)**

.. code:: rust

    let mut my_text = String::new();

    my_text.push('!');         // Append a char
    my_text.push_str(" foo");  // Append a &str

.. note::

  Because :rust:`String` owns its buffer, it may reallocate as it grows

-----------------------
Length vs. Characters
-----------------------

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

------------------------------
Strings vs. Character Arrays
------------------------------

.. list-table::
  :header-rows: 1
  :stub-columns: 1

  * -
    - **Encoding**
    - **Size**
    - **Memory Type**

  * - :rust:`String`
    - UTF-8
    - 1-4 bytes/char
    - Heap-allocated (growable)

  * - :rust:`[char; N]`
    - UCS-4
    - 4 bytes/char
    - Stack-allocated (fixed)

* :rust:`String` better for large ASCII text

  * :rust:`[char;N]` always requires 4 bytes per character

* :rust:`[char;N]` faster for random (direct) access

  * :rust:`String` needs to search content from the beginning
