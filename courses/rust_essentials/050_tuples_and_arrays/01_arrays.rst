========
Arrays
========

-------------------
What Is an Array?
-------------------

* **Definition:** a collection of elements

   * ... of the **same type**
   * ... stored in contiguous memory
   * ... indexed using a discrete range

.. image:: array_diagram.svg
--------------------
Basics
--------------------

- Allocated on the stack - they're fast!
- *Length* is **fixed** and defined at **compile-time**
- *Length* of an array is part of its type
  - :rust:`[T;N]` holds :rust:`N` elements of type :rust:`T`
  - :rust:`[u8;3]` and :rust:`[u8;4]` are considered two different types
- Index starts at :rust:`0` (range is :rust:`0` to :rust:`N-1`)

.. code:: rust

      // Array of 3 elements of type i8   
      let mut values: [i8; 3] = [2, 3, 4];        
      values[2] = 5; // Accessing and modifying an element

.. note::

   Remember, :rust:`mut` is required for modification

-----------------------------------
Safety and Initialization
-----------------------------------

**Safety**

  - Compile-time and run-time **out-of-bounds** checks
  - Accessing an element beyond the defined length will cause a **panic**

**Initialization**

* Can be assigned values either using *array literals*:

.. code:: rust
      
  let integers = [1, 2, 3, 4, 5];          // Integer literals
  let floats   = [1.1, 2.2, 3.3];          // Float literals
  let strings  = ["Hello", "World"];       // String literals
  let bools    = [true, false, true];      // Boolean literals

* Or using *array repeat expression* :rust:`[value; N]`:
  * With length :rust:`N`, known at compile-time
  * Where every element is *value*

.. code:: rust

  let elements: [i8; 5] = [0; 5];

* Assignment is **not** limited to literals
  * Can use variables, function calls, or expressions

-------------------
Iteration
-------------------

* :rust:`for` statement natively supports iterating over arrays

* Given an array

  .. code:: rust

    let primes = [2, 3, 5, 7, 11, 13, 17];  

* Iteration by value would look like

  .. code:: rust

    for prime in primes {
      println!("{}", prime);    
    }

* While index-based looping would look like

  .. code:: rust

    for ii in 0..primes.len() {
      println!("{}", primes[ii]);
    }

.. container:: speakernote

   Arrays do not implement the default Display trait ({}).
   You must use the Debug format parameter:
   println!("a: {:?}", a);
   println!("a: {:#?}", a); (for "pretty printing")

   Testing Macros:
   assert_ne!: Checks that two values are not equal
   assert_eq!: Checks that two values are equal
   These are always checked; use debug_assert! variants for debug-only checks
