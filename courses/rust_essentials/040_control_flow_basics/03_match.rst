===========================
"match" Expressions
===========================

------------------------------
Using "match" as a Statement
------------------------------

- Checks a value against one or more options (*arms*)
- Evaluated top to bottom
  - First arm that matches has its corresponding body executed
- No fall-through between arms
  - Like :ada:`case` in Ada, but unlike :cpp:`switch` in C/C++ 
- If an arm is a single expression, the :rust:`{ }` are **optional**

.. code:: rust

    let belly_rubs = 2; 
    match belly_rubs {
        0 => {
          println!("Grumble");
          println!("Must protest");
        } // Comma is optional here, usually omitted
        1 => {
          println!("Tail wag engaged")
        } // Block is valid but unnecessary for one line
        2 => println!("Happiness!"), // Comma is REQUIRED here
        _ => println!("Suspicion"),  // Trailing comma is allowed, idiomatic
    }

:command:`Happiness!`

--------------------------------
The "match" Must Be Exhaustive
--------------------------------

- Must cover all possibilities

  - Can have a default case :rust:`_`

.. code:: rust

  match count {
      1 => println!("One"),
      2 => println!("Two"),     
      _ => println!("Other!"), // Catches all other possibilities
  }

- Use :rust:`|` to match several values to one arm

.. code:: rust

  match belly_rubs {
      1 | 2 => println!("Not enough."),  // Matches 1 or 2
      3 | 4 | 5 => println!("Perfect!"), // Matches 3 or 4 or 5
      _ => println!("Suspicion."),
  }

--------------------------------------
Using "match" as an Expression
--------------------------------------

- Entire match expression evaluates to a value
- Every arm must return the exact same type
- Inclusive and exclusive ranges

.. list-table::
  :header-rows: 1

  * - **Range**
    - **From**
    - **Up To**

  * - *low..=high*
    - low
    - high (inclusive)
   
  * - *low..high*
    - low
    - high (exclusive)

  * - *..high*
    - mininum possible value
    - high (exclusive)

  * - *low..*
    - low (inclusive)
    - maximum possible value

.. code:: rust

    let temperature_c = 35;
    let current_mood = match temperature_c {
        ..=0 => "Hibernation",
        1..10 => "Need a scarf",
        10..30 => "Perfect",
        30..40 => "Melting!",        
        40.. => "This reading is impossible.",
    };
    println!("Current mood: {}", current_mood);

:command:`Current mood: Melting!`
