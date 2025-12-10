========
Arrays
========

--------------------
Arrays: The Basics
--------------------

- Arrays have multiple elements of the **same type**
- The size is **fixed** and defined at **compile-time**
- The length of an array is part of its type
  - :rust:`[T;N]` holds :rust:`N` elements of type T
  - :rust:`[u8;3]` and :rust:`[u8;4]` are considered two different types
   
.. code:: rust

      // Declaration of an array of 10 elements of type i8
      // All elements are initialized to the value 42
      let mut a: [i8; 10] = [42; 10];
      // Accessing and modifying an element
      // 'mut' is required for modification
      a[5] = 0;
      
-----------------------------------
Arrays: Safety and Initialization
-----------------------------------

Safety

  - Array accesses are **checked at runtime** for being out-of-bounds
  - Accessing an element beyond the defined length will cause a *panic*
    - A *panic* is a form of program termination

Initialization

   - Arrays can be assigned values using literals 
     - e.g., :rust:`[2, 3, 5, 7, 13]` (note the **,**)
   - :rust:`[value; N]` can be used to initialize an array (note the **;**)
     -  with size *N* where every element is *value*

-------------------
Arrays: Iteration
-------------------

Looping over Arrays

  - The :rust:`for` statement natively supports iterating over arrays

.. code:: rust

    let primes = [2, 3, 5, 7, 11, 13, 17];    
    for prime in primes {
        // ... doing something
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
