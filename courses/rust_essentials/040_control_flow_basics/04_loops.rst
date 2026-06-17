=======
Loops
=======

-------------------
"while" Statement
-------------------

- Condition is checked **before** each iteration
- Parentheses around the condition are **optional** 
  - Usage is considered unidiomatic
- Executes the block while the condition is :rust:`true`

.. code:: rust

    let mut countdown = 2;
    while countdown > 0 {
        println!("T-minus {}...", countdown);
        countdown = countdown - 1;
    }
    println!("LIFTOFF!");

.. code:: output

  T-minus 2...
  T-minus 1...
  LIFTOFF!

-----------------
"for" Statement
-----------------

**Iterates over range of values or elements of a collection**

.. code:: rust

    for index in 1..5 { 
        // 4 iterations
    }

    for index in 1..=5 { 
        // 5 iterations
    }

    for element in [1, 2, 3, 4, 5] {
        // 5 iterations
    }

-------------------
"loop" Expression
-------------------

**Loops forever, or until a** :rust:`break`

.. code:: rust
    
    let mut count = 0;
    loop {
        count += 1;
        println!("Count: {count}");
        if count > 2 {
            break;
        }
    }

.. code:: output

  Count: 1
  Count: 2
  Count: 3
