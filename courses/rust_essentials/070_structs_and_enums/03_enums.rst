=======
Enums
=======

--------
Basics
--------

- :rust:`enum` can be one of several distinct **variants**
- **Variants** are accessed using the :rust:`::` notation
  - Called **path separator**
  - Commonly referred to as **scope resolution operator**

.. code:: rust

   enum Direction {
       Left,
       Right,
   }
   let dir = Direction::Left;  

-----------------
Enums with Data
-----------------

- **Variants** can optionally hold data 
  - This is an :rust:`enum` superpower!
- Can't be recursive
  - Type would not be **Sized**
- Similar to *tagged unions* in C++
  - But Rust *enums* are a core feature 

.. code:: rust

    enum PlayerMove {
        Pass,                          // Simple variant
        Run(Direction),                // Tuple variant
        Teleport { xx: u32, yy: u32 }, // Named-field struct variant
    } 
    let move = PlayerMove::Run(Direction::Left);
   
    let teleport = PlayerMove::Teleport { xx: 10, yy: 10 };
    
---------------------
Enum Initialization
---------------------
    
- **Must** specify entire variant when creating :rust:`enum` variable

  - A variant must be selected

  - Data must be initialized if variant holds data

.. code:: rust

    enum Message {
        Quit,                       
        Move { coord_x: i32, coord_y: i32 },    
        ChangeColor(i32, i32, i32), 
    }

    let white = Message::ChangeColor(255,255,255); // OK
    
    // Error! You must provide the content of 'Move'
    let no_color = Message::Move; 
    
.. container:: latex_environment footnotesize    

    :error:`error[E0533]: expected value, found struct variant 'Message::Move'`
    
----------------------
Idiom: State Machine
----------------------

Each variant is **mutually exclusive**

.. code:: rust

    // Represent distinct states of a network connection
    enum ConnectionState {
        // Unit variant (no data)
        Idle,
        // Struct variant (contains connection data)
        Connected {
            session_id: u64, 
            curr_ip: IpAddressV4,
        }, 
        // Tuple variant (contains error data)
        Failed(u16),    
    }