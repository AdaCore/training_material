===========================
:rust:`match` Expressions
===========================

------------------------------------
Using :rust:`match` as a Statement
------------------------------------

- Checks a value against one or more options (arms)
- Evaluation of the :rust:`match` arms from top to bottom
  - First one that matches has its corresponding body executed
- Needs to be exhaustive
  - Either cover all possibilities
  - Or have a default case such as :rust:`_`
- No fall-through between arms
  - Unlike :cpp:`switch` in *C/C++*, like :ada:`case` in *Ada*
- If an arm is a single expression, the :rust:`{ }` are optional

.. code:: rust

  fn main() {
      let belly_rubs = 3; 
      match belly_rubs {
          0 => println!("Grumble. Must protest."),
          1 => println!("Tail wag engaged."),
          2 => println!("Maximum happiness!"),
          3 | 4 => println!("Too much love! Must nap."),
          _ => println!("Suspicion. Where are the treats?"),
      }
  }

--------------------------------------
Using :rust:`match` as an Expression
--------------------------------------

- The entire match expression evaluates to a value
  - Can be used in:
    - assignments
    - function returns
    - other expressions
- Every single arm must return the exact same type as all other arms

.. code:: rust

  fn main() {
      let temperature_c = 35;
      let current_mood = match temperature_c {
          0 => "Hibernation protocol initiated.",
          1..=10 => "Need a scarf.",
          11..=25 => "Perfect.",
          26..=30 => "Slightly sticky.",
          31..=40 => "Melting!",        
          _ => "This reading is impossible.",
      };
      println!("Current mood: {}", current_mood);
  }

