=====================
Destructuring Enums
=====================

----------------------
Accessing Inner Data
----------------------

- Unlike structs, enum data can't be accessed via the dot operator (e.g., no :rust:`result.msg`)

- **Unwrapping** - must use a pattern to "unwrap" the variant and bind its internal data to a variable name

- **Type Safety** - patterns allow you to 

    - Inspect the structure of your types 
    - Ensure every possible state is handled

- **Binding** - the variable name you provide in the pattern (like :rust:`msg`) becomes available **only**
    within that specific match arm

.. code:: rust

    enum Result {
        Ok(i32),
        Err(String),
    }

    fn main() {
        let res = Result::Err(String::from("Connection failed"));

        match res {
            // Extracts the integer from Ok and binds it to 'val'
            Result::Ok(val) => println!("Success: {val}"),

            // Extracts the String from Err and binds it to 'msg'
            Result::Err(msg) => println!("Error: {msg}"),
        }
    }

------------------------
Advanced Enum Patterns
------------------------

- **Nested Matching** - match on the specific values *inside* an enum variant for more granular logic

- **"At" (:rust:`@`) Binding** - allows you to test a value against a pttern while simultaneously keeping the original
    value bound to a varaible

- **Wildcards** - use :rust:`_` to match any variant without binding its internal data 

    - Useful for ignoring specific error types

.. code:: rust

    enum Message {
        Quit,
        Move { x: i32, y: i32 }, // Nested Struct-style variant
        Write(String),
    }

    fn main() {
        let msg = Message::Move { x: 10, y: 20 };

        match msg {
            // Destructuring a Struct-style variant
            Message::Move { x, y: 0 } => println!("Moving on the X axis to {x}"),
            
            // Using @ to test a value AND keep it
            // Matches if the string length is > 0, binds the whole string to 'text'
            Message::Write(text) if !text.is_empty() => {
                println!("Writing: {text}");
            }

            // Catch-all for Quit or other Move variants
            _ => println!("Ignoring message"),
        }
    }
