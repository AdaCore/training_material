=======
Enums
=======

-------------
Basic Enums
-------------

-  Type that can be one of several possible, distinct **variants**
-  Represents a choice between different values
-  **Variants** from an :rust:`enum` are accessed using the '::' notation
   -  called **Path Separator**


.. code:: rust

   enum Direction {
       Left,
       Right,
   }
   let mydir = Direction::Left;  


-----------------
Enums with Data
-----------------

-  **Variants** can optionally hold data - this is an :rust:`enum` superpower!
-  Can't be recursive

.. code:: rust

   enum PlayerMove {
       Pass,                        // Simple variant
       Run(Direction),              // Tuple variant
       Teleport { x: u32, y: u32 }, // Struct variant
   }
   let player_move: PlayerMove = PlayerMove::Run(Direction::Left);
   
   enum Message {
	   Quit,                       // No data
	   Move { x: i32, y: i32 },    // Anonymous struct
       ChangeColor(i32, i32, i32), // Three i32 values
	}
	let msg = Message::Move { x: 10, y: 10 };
	
---------------------
Enum Initialization
---------------------
	
- **Must** specify the entire variant when creating an :rust:`enum` variable

.. code:: rust

   enum Message {
	   Quit,                       // No data
	   Move { x: i32, y: i32 },    // Anonymous struct
       ChangeColor(i32, i32, i32), // Three i32 values
	}

	let msg_1 = Message::ChangeColor(255,255,255); // OK
	// Error! You must provide the content of ChangeColor
	let msg_2 = Message::ChangeColor; 
	
----------------------
Idiom: State Machine
----------------------

-  Each variant is mutually exclusive

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