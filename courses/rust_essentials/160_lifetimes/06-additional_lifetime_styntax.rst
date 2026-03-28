============================
Additional Lifetime Syntax
============================

---------------------------
Additional Lifetime Forms
---------------------------

- Most lifetimes are inferred automatically

- You may encounter a few additional forms in Rust code

.. code:: rust

  &'static str
  &'_ str

- These do not introduce new lifetime rules

- They are variations on concepts you already know

-----------------
Static Lifetime
-----------------

- :rust:`'static` means the data lives for the entire program

.. code:: rust

  let s: &'static str = "hello";

- String literals are stored for the duration of the program

- A reference with :rust:`'static` never becomes invalid

----------------------
Placeholder Lifetime
----------------------

- :rust:`'_` is a placeholder for an inferred lifetime

.. code:: rust

  fn iter(&self) -> Iter<'_, T>

- It tells the compiler to determine the lifetime

- Equivalent to omitting the lifetime when possible
