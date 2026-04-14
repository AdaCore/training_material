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
  
:command:`Box value is 5`
 
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
  
    :error:`error[E0072]: recursive type 'Doll' has infinite size`

- :rust:`Box<T>` provides a pointer with known size

  - Breaks direct recursion loop in memory

    .. code:: rust

      // WORKS: The "Box" is just a pointer to the next doll
      enum Doll {
        Inside(Box<Doll>),
        Empty,
      }
      let a_doll = Doll::Inside(Box::new(Doll::Empty));
      let last_doll = Doll::Empty;

----------------------
Handling Large Data
----------------------

- :rust:`Box<T>` allows to move ownership of data

  - Rather than copying data passed in parameters for function calls
  
    - Useful for large data
	
.. code:: rust	
	
  struct BigData {
    samples: [u64; 1000000], 
    metadata: String,
  }
  let huge_chunk = Box::new(BigData {
    samples: [0; 1000000],
    metadata: String::from("Satellite Telemetry - Region A"),
  });
  // Only reference is moved to the function... 
  // ...not the whole array
  process_data(huge_chunk);
  
---------------------
Resource Management
---------------------

- :rust:`Box<T>` implements :rust:`Drop` to ensure memory safety 

  - Invokes :rust:`Drop` method automatically at end of scope
  
    - No need for manual intervention
	
  - Prevents memory leaks by ensuring deallocation
	
- Transfering ownership is an *O(1)* operation 
  
  - Regardless of what it points to
  
  

  
