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

    let belly_rubs = 3; 
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

--------------------------------
The "match" Must Be Exhaustive
--------------------------------

- Needs to be **exhaustive**
  - Must cover all possibilities
  - Can have a default case such as :rust:`_`

.. code:: rust

  let number = 5;
  match x {
      1 => println!("One"),
      2 => println!("Two"),     
      _ => println!("Other number!"), // Catches all other possibilities
  }

- Use (:rust:`|`) to match several values to one arm.

.. code:: rust

  let belly_rubs = 4;
  match belly_rubs {
      1 | 2 => println!("Not enough love."),    // Matches 1 or 2
      3 | 4 | 5 => println!("Perfect amount!"), // Matches 3 or 4 or 5
      _ => println!("Suspicion."),
  }

--------------------------------------
Using "match" as an Expression
--------------------------------------

- Entire match expression evaluates to a value
- Every arm must return the exact same type
- Inclusive and exclusive ranges with the :rust:`..` and :rust:`..=` syntaxes:
  - *low..=high*: from *low*, up to *high* (inclusive)
  - *low..high*: from *low*, up to *high* (exclusive)
  - *..high*: from minimum possible value, up to *high* (exclusive)
  - *low..*: from *low* (inclusive), up to maximum possible value

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
