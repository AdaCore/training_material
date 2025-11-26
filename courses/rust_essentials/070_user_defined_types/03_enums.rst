=======
Enums
=======

-------------
Basic Enums
-------------

-  An enum is a type that can be one of several possible, distinct :rust:`variants`. 
-  Unlike structs, which combine different values into one, enums represent a choice between different values
-  Variants from an Enum are accessed using the '::' notation


.. code:: rust

   // Basic Enum
   enum Direction {
       Left,
       Right,
   }
   let mydir = Direction::Left;  


-----------------
Enums with Data
-----------------

-  Rust's enums are powerful because their variants can optionally hold data
-  Just like structs they can't be recursive

.. code:: rust

   
   // Enum with Data
   enum PlayerMove {
       Pass,                        // Simple variant
       Run(Direction),              // Tuple variant
       Teleport { x: u32, y: u32 }, // Struct variant
   }
   // How you use it:
   let player_move: PlayerMove = PlayerMove::Run(Direction::Left);
   
   enum Message {
	   Quit,                       // No data
	   Move { x: i32, y: i32 },    // Anonymous struct
       Content(String),            // Single String value
       ChangeColor(i32, i32, i32), // Three i32 values
	}
	// How you use it:
	
	let msg = Message::Move { x: 10, y: 10 };
	
---------------------
Enum initialization
---------------------
	
- You must specify the entire variant when creating an enum value

.. code:: rust

   enum Message {
	   Quit,                       // No data
	   Move { x: i32, y: i32 },    // Anonymous struct
       Content(String),            // Single String value
       ChangeColor(i32, i32, i32), // Three i32 values
	}

	// Assuming enum Message from before
	let msg_1 = Message::Content(String::from("Hi")); // OK
	// Error! You must provide the String.
	let msg_2 = Message::Content; 
	
-----------------------
Idiom : State Machine
-----------------------

-  Each variants are mutually exclusive
   -  Connexion can only be in one of these states at a time

.. code:: rust

	// An enum to represent the distinct states 
	// of a network connection
	enum ConnectionState {
		Idle,           // Unit variant (no data)
		Connecting,     // Unit variant
		Connected {
			// Struct variant (contains data)
			session_id: u64, 
			ip_address: String,
		},
		// Tuple variant (contains data: error code)
		Failed(u16),    
	}