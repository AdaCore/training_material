=====
Vec
=====

-------------------
What Is "Vec<T>"?
-------------------

* :rust:`Vec<T>` is a **growable** **heap-allocated** array

  * Holds a sequence of values of type T on the heap

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

    let mut v = Vec::new();
    v.push(1);

* Using macros (canonical method)

  .. code:: rust

    // macro with initial values
    let v2 = vec![1, 2, 3];

    // 10 zeroes
    let v3 = vec![0; 10];

------------------
Basic Operations
------------------

* Vectors support common operations:

  * **Safe access:** :rust:`v.get(i)` returns :rust:`Option<&T>`
  * **Remove last:** :rust:`v.pop()` returns :rust:`Option<T>`
  * **Iterators:** iterate with :rust:`for x in &v`
  * **Indexing:** :rust:`v[i]`

    * Panics if out of bounds

    .. code:: rust

      let v = vec![1, 2, 3]; // A Vec<i32>
      println!("The second element is: {}", v[1]);
      println!("The sixth element is: {}", v[5]);

    :command:`The second element is: 2`

    :error:`index out of bounds: the len is 3 but the index is 5`

