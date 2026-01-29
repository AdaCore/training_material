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
Slice Creation
-----------------

- Created by referring to a collection, and specifying the range:
  - :rust:`&a[0..len]`: Explicit start and end, *len* excluded
  - :rust:`&a[..len]`: Implicit start to explicit end, *len* excluded
  - :rust:`&a[1..]`: Explicit start to implicit end
  - :rust:`&a[..]`: Full range

.. code:: rust

  let terminator: [char; 4] = ['T', '8', '0', '0'];
  let version: &[char] = &terminator[1..];
  let generation: &[char] = &version[..1];
  let arnold: &[char] = &terminator[..];
  let james: &[char] =  &arnold[2..4];

* Generates the following output:
:command:`terminator:      ['T', '8', '0', '0']`

:command:`version:         ['8', '0', '0']`

:command:`generation:      ['8']`

:command:`arnold:          ['T', '8', '0', '0']`

:command:`james:           ['0', '0']`

-------------------
The "Fat Pointer"
-------------------

- Slices are sometimes called **Fat Pointers**
- They carry a memory address, *where the data starts*
  - And extra "weight", *how many items to look at*

.. note::

    Slices are lightweight and fast "windows" into heavy data

