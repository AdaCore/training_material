==========
"Box<T>"
==========

------------------
What Is "Box<T>"
------------------

- Allocates data on the heap (via :rust:`Box::new`)

  - Stores a fixed-size pointer on the stack

  - Retains single ownership of heap data

- Deallocates memory automatically when object goes out of scope

- Defined in **prelude**

.. code:: rust

  // 'Box::new()' is used to allocate data
  let my_box = Box::new(5);

  // Implicit dereference
  println!("Box value is {}", my_box);

.. code:: output

  Box value is 5

------------------------------------
Using "Box<T>" for Recursive Types
------------------------------------

- Types must have a known size at compile time

  - Recursive types don't have a known size

    .. code:: rust

      // FAILS: How big is an infinite doll?
      enum Doll {
        Inside(Doll),
        Empty,
      }

    .. code:: error
      :font-size: small

      error[E0072]: recursive type 'Doll' has infinite size

- :rust:`Box<T>` provides a pointer with known size

  - Breaks direct recursion loop in memory

    .. code:: rust

      // WORKS: The 'Box' is just a pointer to the next doll
      enum Doll {
        Inside(Box<Doll>),
        Empty,
      }
      let a_doll = Doll::Inside(Box::new(Doll::Empty));
      let last_doll = Doll::Empty;

---------------------
Handling Large Data
---------------------

- :rust:`Box::new(large_value)` can still require a large stack temporary

  - Do not rely on compiler optimizations to avoid stack overflow

- Initialize the elements on the heap with :rust:`vec!`

  - Convert the vector into an owned slice with :rust:`into_boxed_slice()`

.. code:: rust

  fn create_data() -> Box<[u64]> {
    vec![0_u64; 1_000_000].into_boxed_slice()
  }

- Returning the box transfers ownership; the elements stay in place

.. note::

  This avoids a large stack temporary, but heap allocation can still fail

----------------------------------
Choosing a Heap-Allocated Buffer
----------------------------------

- :rust:`Vec<u64>` already owns its heap buffer and is cheap to move

  - Keep the vector if the buffer needs to grow or shrink

- :rust:`Box<[u64]>` owns a heap-allocated slice with a fixed length

  - The length is stored at runtime, unlike :rust:`Box<[u64; 1_000_000]>`

  - A fixed length does not make the elements immutable

- Moving either container does not relocate its heap-allocated elements

  - Moving a large inline value may still relocate its bytes

.. note::

  :rust:`into_boxed_slice()` may reallocate to discard excess capacity, this conversion is different from moving an existing box

------------------------------------
Borrowing or Transferring Ownership
------------------------------------

- Borrow with :rust:`&[u64]` when temporary access is enough

  - The caller retains ownership; the buffer is not copied

- Move :rust:`Box<[u64]>` when another value must own the buffer

  - Ownership is transferred without copying the buffer

.. code:: rust

  fn inspect_data(samples: &[u64]) {
    println!("{} samples", samples.len());
  }
  struct DataProcessor {
    samples: Box<[u64]>,
  }

  let samples = create_data();
  inspect_data(&samples); // Borrow; 'samples' is still usable

  let processor = DataProcessor { samples }; // Move the box
  // 'samples' is no longer usable; 'processor' owns the buffer
  println!("{} samples", processor.samples.len());

.. note::

  Dropping :rust:`processor` drops its boxed slice and frees the buffer, :rust:`&samples` coerces to :rust:`&[u64]`

---------------------
Resource Management
---------------------

- :rust:`Box<T>` implements :rust:`Drop` to ensure memory safety

  - Invokes :rust:`Drop` method automatically at end of scope

    - No need for manual intervention

  - Prevents memory leaks by ensuring deallocation

- Transferring ownership is an *O(1)* operation

  - Regardless of what it points to
