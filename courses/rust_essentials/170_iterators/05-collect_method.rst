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

  fn is_digit(c: &char) -> bool { c.is_numeric() }

  let numbers = vec![1, 2, 2, 3];
  let letters = "Value is 1234";

  // Collect into a 'Vec' (keeps order and duplicates)
  let my_vector: Vec<_> = numbers.iter().collect();

  // Collect into a 'String' (only digits)
  let my_string: String = letters.chars()
                                 .filter(is_digit)
                                 .collect();

.. note::

  :rust:`my_vector` and :rust:`my_string` contain **references** to
  the elements in their sources

---------------------
Collecting "Result"
---------------------

* :rust:`collect()` combines many :rust:`Result<T, E>` values

  * Produces a single :rust:`Result<Vec<T>, E>`

* Stops on first :rust:`Err`

* Otherwise returns :rust:`Ok(Collection)`

.. code:: rust

  let bad_strings = vec!["1", "2", "not_a_number"];
  let good_strings = vec!["1", "2", "42"];

  let bad_numbers: Result<Vec<i32>, _> = bad_strings
      .into_iter().map(|s| s.parse::<i32>())
      .collect();
  println!("bad_numbers: {:?}", bad_numbers);
    
  let good_numbers: Result<Vec<i32>, _> = good_strings
      .into_iter()
      .map(|s| s.parse::<i32>())
      .collect();
  println!("good_numbers: {:?}", good_numbers);

:command:`bad_numbers: Err(ParseIntError { kind: InvalidDigit })`
:command:`good_numbers: Ok([1, 2, 42])`
