==========
"Box<T>"
==========

------------------
What Is "Box<T>"
------------------

- :rust:`Box<T>` provides unique ownership of a value stored on the heap
  - The :rust:`Box<T>` value itself has a known, fixed size
  - Moving the box transfers ownership without moving the heap allocation

- The heap allocation is released automatically when the box is dropped

- Defined in **prelude**

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

  - The recursive value itself is stored behind the box

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

- :rust:`Box<T>` gives unique ownership of heap data
  - Moving the box keeps the heap allocation in place

- Avoid :rust:`Box::new([0; LARGE_SIZE])` for large arrays
  - Creating it can still require a large stack temporary
  - Do not rely on optimization to remove it

- Use :rust:`vec!` for large buffers
  - Keep :rust:`Vec<T>` if the buffer must resize
  - Convert to :rust:`Box<[T]>` for a fixed-length buffer

.. code:: rust

  fn create_data() -> Box<[u64]> {
    vec![0_u64; 1_000_000].into_boxed_slice()
  }

.. note::

  :rust:`into_boxed_slice()` may reallocate to discard excess capacity

------------------------------------
Borrowing or Transferring Ownership
------------------------------------

- Borrow the boxed slice when temporary access is enough
  - The caller retains ownership
  - The buffer is not copied
- Move the :rust:`Box<[u64]>` when another value must own it
  - Ownership is transferred
  - The heap allocation stays in place

.. code:: rust

  struct DataProcessor {
    samples: Box<[u64]>,
  }

  let samples = create_data();

  let processor = DataProcessor { samples }; // Move the box
  // 'samples' is no longer usable
  println!("{} samples", processor.samples.len());

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

- Moving a :rust:`Box<T>` is an *O(1)* operation
  - The heap allocation stays in place
