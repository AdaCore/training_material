===========
Lifetimes
===========

---------------------
What is a Lifetime?
---------------------

- Span of code during which a reference is valid

  - Not to be confused with **scope** of variables!

- Ends the last time its reference is used

- Exists even when not written

.. code:: rust
  :number-lines: 1

  {
    let treasure = "gold";
    let treasure_map = &treasure;
    println!("{treasure_map}");
    println!("{treasure}");
  }

:command:`gold`
:command:`gold`

- :rust:`treasure` is in scope from lines 2 – 5
- :rust:`treasure_map` lifetime is from lines 3 – 4

----------------------
Why Lifetimes Matter
----------------------

**References point to data owned elsewhere**

- If data is dropped while a reference still exists...

  - ...the program would have a **dangling reference**

- A reference cannot outlive the value it refers to

  - Enforced at **compile time**

----------------------
Lifetime Enforcement
----------------------

- Borrow checker verifies references remain valid

  - Reference Lifetime <= Value Lifetime

- Prevents

  - Dangling references

  - Use-after-free

  - Invalid memory access

- No garbage collector required!
