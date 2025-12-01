===========================
"match" Expressions
===========================

------------------------------
Using "match" as a Statement
------------------------------

- Checks a value against one or more options (*arms*)
- Evaluation of the :rust:`match` arms from top to bottom
  - First one that matches has its corresponding body executed
- No fall-through between arms
  - Unlike :cpp:`switch` in *C/C++*, like :ada:`case` in *Ada*
- If an arm is a single expression, the :rust:`{ }` are optional

.. code:: rust

    let belly_rubs = 3; 
    match belly_rubs {
        0 => println!("Grumble. Must protest."),
        1 => println!("Tail wag engaged."),
        2 => println!("Maximum happiness!"),
        _ => println!("Suspicion. Where are the treats?"),
    }

--------------------------------
The "match" Must Be Exhaustive
--------------------------------

- Needs to be **exhaustive**
  - Either cover all possibilities
  - Or have a default case such as :rust:`_`

.. code:: rust

  let x = 5;
  match x {
      1 => println!("One"),
      2 => println!("Two"),     
      _ => println!("Other number!"),  // Catches all other possibilities
  }

- Use (:rust:`|`) to match several values to one arm.

.. code:: rust

  let belly_rubs = 4;
  match belly_rubs {
      0 | 1 => println!("Not enough love."),    // Matches 0 OR 1
      2 | 3 | 4 => println!("Perfect amount!"), // Matches 2 OR 3 OR 4
      _ => println!("Suspicion."),
  }

--------------------------------------
Using "match" as an Expression
--------------------------------------

- The entire match expression evaluates to a value
- Every arm must return the exact same type
- Use the :rust:`..=` syntax for inclusive range

.. code:: rust

    let temperature_c = 35;
    let current_mood = match temperature_c {
        0 => "Hibernation protocol initiated.",
        1..=10 => "Need a scarf.",
        11..=25 => "Perfect.",
        26..=30 => "Slightly sticky.",
        31..40 => "Melting!", // 40 is not included!        
        _ => "This reading is impossible.",
    };
    println!("Current mood: {}", current_mood);
