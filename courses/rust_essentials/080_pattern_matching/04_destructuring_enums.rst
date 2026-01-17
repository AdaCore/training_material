=====================
Destructuring Enums
=====================

--------------------
Enums and Variants
--------------------

- Enums represent a value that is exactly one of several possibilities

- Variant names *qualify* the pattern

  - Identify which specific structure is being matched

- Each variant can store different types and amounts of data

.. code:: rust

  enum Message {
    Quit,                       // No data
    Move { x: i32, y: i32 },    // Named fields
    Write(String),              // Tuple data
  }

.. note::

  Patterns are the ONLY way to safely access the data inside these variants

------------------------
Matching Enum Variants
------------------------

- Enum patterns match specific variants

- Each variant is handled explicitly

- Variant names *qualify* patterns

.. code:: rust

  enum Message {
    Quit,
    Write(String),
  }

  let msg = Message::Quit;

  match msg {
    Message::Quit => println!("quit"),
    Message::Write(text) => println!("write: {}", text),
  }

----------------------------
Destructuring Variant Data
----------------------------

- Patterns can extract data from variants

- Payloads are bound directly in the pattern

- Binding occurs only when the variant matches

- Use :rust:`_` or :rust:`..` to ignore specific variant data

.. code:: rust

  let msg = Message::Write(String::from("hello"));

  match msg {
    // Capture: Binding the payload to 'text'
    Message::Write(text) => println!("text: {text}"),

    // Ignore: Using '_' because we don't need the string
    // Note: You must keep the parens for variants with data
    Message::Write(_) => println!("Received a message, but ignoring content"),

    // Ignore All: Using '..' for complex variants
    Message::Move { .. } => println!("System is moving"),

    Message::Quit => println!("quit"),
  }

----------------
Tuple Variants
----------------

- Contain unnamed fields

- Data is matched positionally

- Patterns mirror the variant structure

.. code:: rust

  enum Event {
    KeyPress(char),
    MouseClick(i32, i32),
  }

  let touch = Event::MouseClick(10, 20);

  match touch {
    Event::KeyPress(tap) => println!("key: {}", tap),
    Event::MouseClick(x, y) => 
      println!("click at {}, {}", x, y),
  }

-----------------
Struct Variants
-----------------

- Use named fields

- Patterns match fields by name

- Partial matching is supported

.. code:: rust

  enum Shape {
    Circle { radius: f64 },
    Rectangle { width: f64, height: f64 },
  }

  let profile = Shape::Rectangle { width: 3.0, height: 4.0 };

  match profile {
    Shape::Circle { radius } => println!("circle r={}", radius),
    Shape::Rectangle { width, height } => {
      println!("rect {} x {}", width, height);
    }
  }

---------------------------
Exhaustiveness with Enums
---------------------------

- All enum variants must be handled

- Missing variants cause a compile-time error

- Exhaustiveness scales as enums evolve

----------------------
Enums as Data Models
----------------------

- Enums represent mutually exclusive states

- Each variant defines its own shape

- Pattern matching enforces correct handling
