=====
Vec
=====

-------------------
What Is "Vec<T>"?
-------------------

* :rust:`Vec<T>` is a **growable** **heap-allocated** array

  * Holds a sequence of values of type :rust:`T` on the heap

    * Size can grow/shrink at runtime

  * Generic over element type: :rust:`Vec<i32>`, :rust:`Vec<String>`, etc.
  * Elements are contiguous in memory

* Dereferences to a slice (:rust:`[T]`)

  *  All slice methods apply

------------------
Creating Vectors
------------------

* Creating a vector using :rust:`new()`

  .. code:: rust

    let mut simple_vector = Vec::new();
    simple_vector.push(1);

* Using the :rust:`vec!` macro

  .. code:: rust

    // Explicit list
    let list_of_values = vec![1, 2, 3];

    // Repeat expression
    let zeroes = vec![0; 10]; // Value "0" 10 times

    // List of 'i8'
    let bytes_1 = vec![1_i8, 2, 3];
    let bytes_2: Vec<i8> = vec![1, 2, 3];
  
------------------
Basic Operations
------------------

* Accessing items in the array

  * **Safe access:** :rust:`v.get(idx)` returns :rust:`Option<&T>`

    * Use :rust:`Some` / :rust:`None` capability to handle :rust:`idx` out of bounds

  * **Remove last:** :rust:`v.pop()` returns :rust:`Option<T>`

    * Also returns :rust:`Option<&T>`

  * **Iterators:** iterate with :rust:`for x in &v`

    * Iterator is of actual type

.. warning::

  **Direct Indexing** :rust:`v[idx]` allowed but not recommended

.. code:: rust

  let v = vec![1, 2, 3];
  println!("The sixth element is: {}", v[5]);

**Runtime Error**

:error:`index out of bounds: the len is 3 but the index is 5`

--------------------
Working with "Vec"
--------------------

.. code:: rust

  let mut colors = vec!["Red", "Green", "Blue"];

  // Iterate over 'colors'
  println!("All colors");
  for c in &colors { println!("  {}", c); }

  // Pop - remove and return the last item
  let last = colors.pop();
  println!("last: {last:?}");

  // Get - safe indexing
  let one  = colors.get(1);
  let five = colors.get(5);

  println!("one: {one:?}");
  println!("five: {five:?}");

:command:`All colors`

:command:`  Red`

:command:`  Green`

:command:`  Blue`

:command:`last: Some("Blue")`

:command:`one: Some("Green")`

:command:`five: None`
