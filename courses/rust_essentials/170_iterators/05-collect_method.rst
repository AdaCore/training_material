==================
"collect" Method
==================

-----------------------
The Ultimate Consumer
-----------------------

* Most iterator methods (like :rust:`map` and :rust:`filter`) are *lazy*

  * Describe a transformation
  * Don't actually do anything

* :rust:`collect()` is the "on switch"

  * Iterates through entire sequence
  * Stores the results in a collection (like a :rust:`Vec` or :rust:`HashMap`)

* Typically use *turbofish* syntax to tell compiler what you want

  .. code:: rust

    collect::<Vec<i32>>()
    collect::<HashSet<_>>()

  * The "_" syntax lets Rust infer the data type automatically

--------------------------
One Method, Many Results
--------------------------

**Same logic can build different structures**

.. code:: rust

  let numbers = vec![1, 2, 2, 3];

      // Collect into a Vector (keeps order and duplicates)
      let v: Vec<_> = numbers.iter().collect();

      // Collect into a HashSet (removes duplicates!)
      use std::collections::HashSet;
      let s: HashSet<_> = numbers.iter().collect();

---------------------
Collecting "Result"
---------------------

* Convert collection of :rust:`Result` types into single :rust:`Result` with a collection

* If *any* item in iterator is :rust:`Err`

  * :rust:`collect` stops and returns :rust:`Err`

* Otherwise returns :rust:`Ok(Collection)`

.. code:: rust

  let bad_strings = vec!["1", "2", "not_a_number"];
  let good_strings = vec!["1", "2", "42"];

  let bad_numbers: Result<Vec<i32>, _> = bad_strings
      .into_iter()
      .map(|s| s.parse::<i32>())
      .collect();
  println!("bad_numbers: {:?}", bad_numbers);
    
  let good_numbers: Result<Vec<i32>, _> = good_strings
      .into_iter()
      .map(|s| s.parse::<i32>())
      .collect();
  println!("good_numbers: {:?}", good_numbers);

:command:`bad_numbers: Err(ParseIntError { kind: InvalidDigit })`
:command:`good_numbers: Ok([1, 2, 42])`
