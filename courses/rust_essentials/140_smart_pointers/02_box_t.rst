==========
"Box<T>"
==========

-------------------
What Is "Box<T>"?
-------------------

- Stores its value on the heap

- Provides unique ownership of that value

- :rust:`Box<T>` itself has a known, fixed size

- Available through the standard **prelude**

.. code:: rust

  // 'Box::new()' is used to allocate data
  let my_box = Box::new(5);

  println!("Box value is {}", my_box);

.. code:: output

  Box value is 5

------------------------------------
Using "Box<T>" for Recursive Types
------------------------------------

- A type cannot contain itself directly

  - Direct recursion would give the type an infinite size

    .. code:: rust

      // FAILS: How big is an infinite doll?
      enum Doll {
        Inside(Doll),
        Empty,
      }

    .. code:: error
      :font-size: small

      error[E0072]: recursive type 'Doll' has infinite size

- :rust:`Box<T>` has a known, fixed size

  - Recursive value itself is stored behind the box

    .. code:: rust

      // WORKS: 'Box<Doll>' gives this recursive field a known size
      enum Doll {
        Inside(Box<Doll>),
        Empty,
      }
      let a_doll = Doll::Inside(Box::new(Doll::Empty));
      let last_doll = Doll::Empty;

---------------------
Handling Large Data
---------------------

- :rust:`Box::new([0; LARGE_SIZE])` requires temporary stack value
  - Heap allocation does not guarantee construction directly on heap
  - Do not rely on optimization to remove the temporary value

- :rust:`vec!` creates large buffers with heap-allocated storage
  - Avoids constructing large array on the stack
  - Keeps :rust:`Vec<T>` if buffer must resize
  - Converts to :rust:`Box<[T]>` for fixed-length buffer

.. code:: rust

  fn create_data() -> Box<[u64]> {
    vec![0_u64; 1_000_000].into_boxed_slice()
  }

.. note::

  - :rust:`into_boxed_slice()` locks the vector into a fixed-length slice
    - drops excess capacity

-------------------------------------
Borrowing or Transferring Ownership
-------------------------------------

- *Borrow* the boxed slice when temporary access is enough
  - Caller retains ownership
  - Buffer is not copied

- *Move* the :rust:`Box<[u64]>` when another value must own it
  - Ownership is transferred
  - Allocated value stays in place

.. code:: rust

  struct DataProcessor {
    samples: Box<[u64]>,
  }

  let data = create_data();

  let processor = DataProcessor { samples: data }; // Move the box
  // 'data' is no longer usable
  println!("Samples: {}", processor.samples.len());

.. code:: output

  Samples: 1000000

.. note::

  Ownership transfer is not a copy operation

---------------------
Resource Management
---------------------

- :rust:`Box<T>` releases its heap allocation automatically when dropped
  - Usually when the owning value goes out of scope
  - No manual deallocation is required

.. code:: rust

  {
    let samples = vec![0_u64; 1_000_000]
        .into_boxed_slice();
    // Use 'samples'...
  } // 'samples' is dropped here

.. note::

  - Moving a :rust:`Box<T>` is an *O(1)* operation
    - Heap allocation stays in place
