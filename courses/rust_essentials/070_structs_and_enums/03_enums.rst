=======
Enums
=======

--------
Basics
--------

-  Can be one of several possible, distinct **variants**
-  Represents a choice between different values
-  **Variants** from an :rust:`enum` are accessed using the '::' notation
   -  Called **path separator**


.. code:: rust

   enum Direction {
       Left,
       Right,
   }
   let dir = Direction::Left;  


-----------------
Enums with Data
-----------------

-  **Variants** can optionally hold data 
   -  this is an :rust:`enum` superpower!
-  Can't be recursive
-  Similar to a union in C/C++

.. code:: rust

   enum Move {
       Pass,                          // Simple variant
       Run(Direction),                // Tuple variant
       Teleport { xx: u32, yy: u32 }, // Named-field struct Variant
   } 
   let move: PlayerMove = PlayerMove::Run(Direction::Left);
   
   enum Message {
	   Quit,                          // No data
	   Move { xx: i32, yy: i32 },     // Anonymous struct
       ChangeColor(i32, i32, i32),    // Three i32 values
	}
	let message = Message::Move { xx: 10, yy: 10 };
	
---------------------
Enum Initialization
---------------------
	
-  **Must** specify the entire variant when creating an :rust:`enum` variable

-  No default values for data

.. code:: rust

   enum Message {
	   Quit,                       
	   Move { coord_x: i32, coord_y: i32 },    
       ChangeColor(i32, i32, i32), 
	}

	let white = Message::ChangeColor(255,255,255); // OK
	// Error! You must provide the content of Move
	let no_color = Message::Move; 
	
----------------------
Idiom: State Machine
----------------------

-  Each variant is **mutually exclusive**

.. code:: rust

	// represent distinct states of a network connection
	enum ConnectionState {
		Idle,           // Unit variant (no data)
		Connected {
			// Struct variant (contains data)
			session_id: u64, 
			curr_ip: IpAddressV4,
		}, 
		// Tuple variant (contains data: error code)
		Failed(u16),    
	}