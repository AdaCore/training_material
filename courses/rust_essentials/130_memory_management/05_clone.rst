=======
Clone
=======

----------------------
Explicit Duplication
----------------------

- Creates a *deep copy* of underlying data
  - Typically duplicating heap-allocated resources

.. code:: rust

  let poodle = String::from("ball"); 
  let yorkie = poodle.clone(); 
  
  println!("{}", poodle); // 'poodle' still has its ball
  println!("{}", yorkie); // 'yorkie' has its own copy

---------------------
Cost of Duplication
---------------------

- Useful when original variable must remain valid after a function call
- Significantly more expensive than a move 
  - Requires new memory allocation and data migration

.. code:: rust

    let t_rex_one = vec![0_u8; 10 * 1024 * 1024]; 
    let t_rex_two = t_rex_one.clone(); 
    println!("T-Rex One: {} bytes", t_rex_one.len());
    println!("T-Rex Two: {} bytes", t_rex_two.len());        

-------------------------
The Clone Away Strategy
-------------------------

  - Prioritize progress over performance by "cloning away" conflicts
    - Deferring optimization until the logic is stable
    - Subsequent refinements can substitute clones with references
      - :rust:`.clone()` serves as a clear visual marker of intentional heap allocation

.. code:: rust

    let agent = String::from("Smith");
    
    // Explicitly clone the data
    say_hello(agent.clone());
    say_hello(agent.clone());
    say_hello(agent.clone());
    say_hello(agent);
    // 'agent' is no longer valid - its value was consumed
