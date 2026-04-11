========
Slices
========

------------------
What Are Slices?
------------------

- View into memory owned by another variable
  - Must be contiguous sequence (like an Array)
- Refer to data stored elsewhere
- Use zero-based indexing
  - Are inclusive of the starting bound but exclusive of the end
    - Using :rust:`..`
  - Unless explicitly marked as inclusive
    - Using :rust:`..=`

.. code:: rust

    let primes: [i32; 6] = [2, 3, 5, 7, 11, 13];  
    let slice: &[i32] = &primes[2..4];

:command:`primes: [2, 3, 5, 7, 11, 13]`

:command:`slice: [5, 7]`

----------------
Slice Creation
----------------

**Created by referring to a collection, and specifying the range**

.. container:: latex_environment scriptsize

.. list-table::
  :header-rows: 1

  * - Syntax
    - Range
    
  * - :rust:`&a[start..]`
    - Explicit start to implicit end

  * - :rust:`&a[..end]`
    - Implicit start to explicit end (*end* excluded)

  * - :rust:`&a[..]`
    - Full range

  * - :rust:`&a[start..end]`
    - Explicit start and end (*end* excluded)

----------------
Slice Examples
----------------

.. code:: rust

  let terminator: [char; 4] = ['T', '8', '0', '0'];

:command:`terminator:      ['T', '8', '0', '0']`

.. code:: rust

  // Slicing the 'terminator' array
  let version: &[char] = &terminator[1..];
  let generation: &[char] = &version[..1];
  let arnold: &[char] = &terminator[..];
  let james: &[char] = &arnold[2..4];
  let terminated = &terminator[0..0];

:command:`version:         ['8', '0', '0']`

:command:`generation:      ['8']`

:command:`arnold:          ['T', '8', '0', '0']`

:command:`james:           ['0', '0']`

:command:`terminated:      []`

.. note::

  Out-of-bounds slicing triggers a compile error if the range is static, or a *panic* if the range is dynamic

-------------
Fat Pointer
-------------

**Slices are sometimes called** *Fat Pointers*

- Carry **two** components

  - **Data Pointer** - memory address where data starts
  - **Length** - how many items to look at

.. code:: rust

  const ARRAY_SIZE: usize = 4_000_000;
  static HUGE_ARRAY: [i32; ARRAY_SIZE] = [0; ARRAY_SIZE];
  let first_five = &HUGE_ARRAY[0..5];

.. note::

    Creating a slice is **O(1)** - it takes the same constant time whether the original array has 4 elements or 4 million

------------------
&str vs "String"
------------------

- :rust:`&str`: **String slice**, immutable reference to UTF-8 encoded bytes
  - Fixed length (cannot grow or shrink)
  - String literals (:rust:`"Hello"`) are :rust:`&str`
- :rust:`String`: Buffer of UTF-8 encoded bytes
  - Allocated on the heap, can grow or shrink

.. code:: rust

   let s1: &str = "Hello World";
   let s2: &str = &s1[..5];
   println!("s2: {s2}");

:command:`s2: Hello`
