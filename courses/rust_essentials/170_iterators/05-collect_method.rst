==================
"collect" Method
==================

-----------------------
The Ultimate Consumer
-----------------------

* Most iterator methods (like :rust:`map` and :rust:`filter`) are *lazy*

  * Describe transformations
  * Don't modify any data

* :rust:`collect()` is the "on switch"

  * Runs entire pipeline
  * Stores results in a collection (:rust:`Vec` or :rust:`HashMap`, etc.)

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
  let my_vector: Vec<_> = numbers.iter().collect();

  // Collect into a HashSet (removes duplicates!)
  use std::collections::HashSet;
  let my_set: HashSet<_> = numbers.iter().collect();

.. note::

  :rust:`my_vector` and :rust:`my_set` contain **references** to
  the items in :rust:`numbers`

---------------------
Collecting "Result"
---------------------

* :rust:`collect()` combines many :rust:`Result<T, E>` values

  * Produces a single :rust:`Result<Vec<T>, E>`

* Stops on the first :rust:`Err`

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
