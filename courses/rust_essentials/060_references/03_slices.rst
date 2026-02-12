========
Slices
========

------------------
What Are Slices?
------------------

- A view into memory owned by another variable
  - Must be a contiguous sequence (like an Array)
- Refer to data stored elsewhere
- Default range (:rust:`..`): First bound inclusive, last bound exclusive
  - Inclusive range (:rust:`..=`): Both bounds are inclusive
  - Indexes start at 0

.. code:: rust

    let primes: [i32; 6] = [2, 3, 5, 7, 11, 13];  
    let slice: &[i32] = &primes[2..4];

    println!("Primes: {primes:?}");
    println!("Slice: {slice:?}");

* Generates the following output

:command:`Primes: [2, 3, 5, 7, 11, 13]`

:command:`Slice: [5, 7]`

----------------
Slice Creation
----------------

- Created by referring to a collection, and specifying the range:
  - :rust:`&a[start..]`: Explicit start to implicit end
  - :rust:`&a[..end]`: Implicit start to explicit end (*end* excluded)
  - :rust:`&a[..]`: Full range
  - :rust:`&a[start..end]`: Explicit start and end (*end* excluded)
  
.. code:: rust

  let terminator: [char; 4] = ['T', '8', '0', '0'];

:command:`terminator:      ['T', '8', '0', '0']`

.. code:: rust

  // Slicing the terminator array
  let version: &[char] = &terminator[1..];
  let generation: &[char] = &version[..1];
  let arnold: &[char] = &terminator[..];
  let james: &[char] =  &arnold[2..4];


:command:`version:         ['8', '0', '0']`

:command:`generation:      ['8']`

:command:`arnold:          ['T', '8', '0', '0']`

:command:`james:           ['0', '0']`

-------------
Fat Pointer
-------------

- Slices are sometimes called **Fat Pointers**
- They carry **two** components:
  - **Data Pointer** - memory address where the data starts
  - **Length** - how many items to look at

.. code:: rust

  static HUGE_ARRAY = [0; 4_000_000];
  let first_five = &HUGE_ARRAY[0..5];

.. note::

    Creating a slice is **O(1)** ; it takes the same constant time whether the original array has 4 elements or 4 million.

