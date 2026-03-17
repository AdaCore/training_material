======================
Lifetime Annotations
======================

---------------------
What is a Lifetime?
---------------------

- The span of code during which a reference is valid

.. code:: rust

  // 'second' must not outlive 'first'
  let first = 10;
  let second = &first;
  println!("{second}");

----------------------
Why Lifetimes Matter
----------------------

- References point to data owned elsewhere

- If data disappears while a reference still exists...

  - ...the program would have a **dangling reference**

- Rust ensures that a reference cannot outlive the value it refers to

  - This rule is enforced at **compile time**

----------------------------------
Lifetimes and the Borrow Checker
----------------------------------

- The borrow checker verifies references remain valid

  - Reference Lifetime <= Value Lifetime

- This prevents

  - Dangling references

  - Use-after-free

  - Invalid memory access

- No garbage collector required!

----------------------
Lifetime Annotations
----------------------

- Rust can name lifetimes explicitly

.. code:: rust

  // Reference to 'str' valid for at least lifetime ''a'
  &'a str

- Lifetimes start with :rust:`'`

.. code:: rust

  // Examples of names:
  'some_name
  'a_lifetime
  'existence

------------------------------
What Lifetime Annotations Do
------------------------------

- Lifetime annotations do not create lifetimes

- They describe relationships between References

- Most lifetimes are inferred automatically by the compiler

- Annotations are only *required* when relationships are ambiguous

.. code:: rust

  // 'left', 'right', and the return value share the same lifetime
  fn choose<'a>(left: &'a str, right: &'a str) -> &'a str
