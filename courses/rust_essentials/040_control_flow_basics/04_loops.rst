=======
Loops
=======

-------------------------
"while" statement
-------------------------

- The condition is checked **before** each iteration
- **Parentheses** around the condition are optional (but considered unidiomatic)
- Works much like in other languages
- Executes the loop block as long as the condition is true

.. code:: rust

    let mut countdown = 10;
    while countdown > 0 {
        println!("T-minus {}...", countdown);
        countdown = countdown - 1;
    }
    println!("LIFTOFF!");

-----------------------
"for" statement
-----------------------

* The :rust:`for` loop iterates over ranges of values or the items in a collection:

.. code:: rust

    for index in 1..5 { // 4 iterations 
        println!("index: {index}");
    }

    for index in 1..=5 { // 5 iterations 
        println!("index: {index}");
    }

    for element in [1, 2, 3, 4, 5] { // 5 iterations
        println!("element: {element}");
    }

-------------------------
"loop" expression
-------------------------

- The :rust:`loop` expression loops forever, until a :rust:`break` 

.. code:: rust
    
    let mut count = 0;
    loop {
        count += 1;
        println!("{count}");
        if count > 100 {
            break;
        }
    }
