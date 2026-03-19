===========
Lifetimes
===========

---------------------
What is a Lifetime?
---------------------

- Span of code during which a reference is valid

.. code:: rust
  :number-lines: 1

  let first = 10;
  let second = &first;
  println!("{second}");

- :rust:`first` lives from lines 1 – 3
- :rust:`second` lives from lines 2 – 3
- :rust:`second` does **not** outlive :rust:`first`

----------------------
Why Lifetimes Matter
----------------------

- References point to data owned elsewhere

- If data is dropped while a reference still exists...

  - ...the program would have a **dangling reference**

- Rust ensures that a reference cannot outlive the value it refers to

  - This rule is enforced at **compile time**

--------------------------------
What Does a Lifetime Describe?
--------------------------------

- How long a reference to data remains valid

- Tied to the referenced data, not the variable name

- Exists even when not written

----------------------------------
Lifetimes and the Borrow Checker
----------------------------------

- Borrow checker verifies references remain valid

  - Reference Lifetime <= Value Lifetime

- This prevents

  - Dangling references

  - Use-after-free

  - Invalid memory access

- No garbage collector required!