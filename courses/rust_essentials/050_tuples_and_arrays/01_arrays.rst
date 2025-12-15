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

- Allocated on the stack, they're fast!
- *Size* is **fixed** and defined at **compile-time**
- *Length* of an array is part of its type
  - :rust:`[T;N]` holds :rust:`N` elements of type T
  - :rust:`[u8;3]` and :rust:`[u8;4]` are considered two different types
- Indexes start at :rust:`0` (Range is :rust:`0` to :rust:`N-1`)

.. code:: rust

      // Array of 3 elements of type i8   
      let mut values: [i8; 3] = [2, 3, 4];        
      values[2] = 5; // Accessing and modifying an element

.. note::

   Reminder: 'mut' is required for modification

-----------------------------------
Safety and Initialization
-----------------------------------

**Safety**

  - Accesses are **checked at runtime** for being out-of-bounds
  - Accessing an element beyond the defined length will cause a *panic*
    - A *panic* is a form of program termination

**Initialization**

* Can be assigned values using literals

.. code:: rust
      
  let integers = [1, 2, 3, 4, 5];          // Integer literals
  let floats   = [1.1, 2.2, 3.3];          // Float literals
  let strings  = ["Hello", "World"];       // String slice literals
  let bools    = [true, false, true];      // Boolean literals

* :rust:`[value; N]` can be used to initialize an array (note the **;**)
  * With size *N* where every element is *value*

.. code:: rust

  let elements: [i8; 5] = [0; 5];

* Assignment is **not** limited to literals
  * Not restricted to hard-coded values
  * Can use variables, function calls, or expressions

-------------------
Iteration
-------------------

  - :rust:`for` statement natively supports iterating over arrays

.. code:: rust

    let primes = [2, 3, 5, 7, 11, 13, 17];  
    for prime in primes {
        // ... do something
    }

    for ii in 0..primes.len() {
        // ... do something, using primes[ii]
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
