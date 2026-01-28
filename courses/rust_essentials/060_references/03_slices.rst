=======================
Slices
=======================

------------------
What Are Slices?
------------------

- Give you a view into a larger collection
  - Any container with data stored in contiguous memory
- Refer to data stored elsewhere

.. code:: rust

    let primes: [i32; 6] = [2, 3, 5, 7, 11, 13];  
    let slice: &[i32] = &primes[2..4];

    println!("Primes: {primes:?}");
    println!("Slice: {slice:?}");

* Generates the following output:

:command:`Primes: [2, 3, 5, 7, 11, 13]`

:command:`Slice: [5, 7]`

-----------------
Creating Slices
-----------------

.. code:: rust

    let terminator: [char; 4] = ['T', '8', '0', '0'];  
    let version: &[char] = &terminator[1..];
    let generation: &[char] = &version[..1];
    let arnold: &[char] = &terminator[..];

- By referring to a collection and specifying the range in brackets
- Range Syntax:
  - :rust:`&a[0..len]`: Explicit start and end
  - :rust:`&a[..len]`: Drop the starting index if it is 0
  - :rust:`&a[2..]`: Drop the last index to include everything up to the end
  - :rust:`&a[..]`: Full slice

-------------------
The "Fat Pointer"
-------------------

- Slices are sometimes called **Fat Pointers**
- They carry a memory address, *where the data starts*
  - And extra "weight", *how many items to look at*

.. note::

    Slices are lightweight and fast "windows" into heavy data

