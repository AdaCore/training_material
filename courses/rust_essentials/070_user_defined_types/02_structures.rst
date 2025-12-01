============
Structures
============

---------
Structs
---------

-  :rust:`struct` creates a type that can hold multiple related values

-  Structs work like in C or C++

-  Can hold any basic types 

-  Fields are accessed using dot notation
   

.. code:: rust

	struct User {
		age: u8,
		number_of_messages : u32,
	}
	
	let myself = User {
		age : 32,
		number_of_messages : 275,	
	}
	
	if myself.number_of_messages > 250 {
		println!("You post too much !");
	}  

------------------------------
Struct of a Struct (or Enum)
------------------------------
   
-  Can hold :rust:`struct` or :rust:`enum` 
   -  But it can't be recursive! 

.. code:: rust

	struct Engine {
		horsepower: u16,
		fuel_type: String,
	}
	struct Car {
		make: String,
		model: String,
		new: bool,
		// The Engine struct is a component of the Car struct
		power_plant: Engine, 
	}
	struct Buyer {
		curr_vehicle: Car,
		// ERROR : this is recursive
		previous_owner: Buyer, 
	}
	
-----------------------
Struct Initialization
-----------------------

-  Initialization of every field of a :rust:`struct` is **mandatory** when you instantiate it 
-  There are no implicit default values
-  If a local variable has the same name as the :rust:`struct` field, name can be written only once

.. code:: rust

	struct User {
		email : String,    
		username : String, 
		active: bool,
		sign_in_count: u64,
	}

	
	fn send (email: String, username: String) -> User {
		User {
			// Same as email: email
			// The field email takes value of send parameter
			email,
			// Same as username: username
			// See above
			username, 
			active: true,
			sign_in_count: 1,
		}
	}

------------------------
Struct Update Operator 
------------------------

-  The '..' operator allows easy creation a new instance of a :rust:`struct` based on another instance 
   -  Specify values only for the fields that needs to change 
   -  All unspecified fields are copied from the instance following the '..'

.. code:: rust

	struct UserSettings {
		theme: String,
		font_size: u8,
		active: bool,
	}
	let base_settings = UserSettings {
		theme: String::from("dark"),
		font_size: 14,
		active: false,
	};
	// only change 'active' to true in 'new_settings'
	let new_settings = UserSettings {
		active: true, // Overridden field
		..base_settings // Copy all other fields (theme, font_size)
	};

---------------
Tuple Structs
---------------

-  Very similar to regular :rust:`struct` 
   -  Useful to give a structure a specific type name without naming all of its fields
-  First element of a tuple is 0 not 1  

.. code:: rust

	struct Color(u8, u8, u8);

	// How you use it:
	let red = Color(255, 0, 0);
	// Displayed result is 0 not 255
	println!("The green channel value is: {}", red.1);
	
---------------
Mutable
---------------

-  Mutability in Rust applies to the entire :rust:`struct` instance
   -  No partial application for some of the fields
   
.. code:: rust

	struct Color(u8, u8, u8);

	let red = Color(255, 0, 0);
	let mut color_mut = red;
	
	// color_mut is set to blue
	color_mut.2 = 255;
	color_mut.0 = 0;	
	
-----------------
Idiom: Newtype
-----------------


-  A :rust:`Newtype` is a tuple :rust:`struct` with a single field. It is used to ensure type safety

   -  :rust:`UserId` and :rust:`MillisecondDuration` are both :rust:`i64` but you can not assign one to the other

.. code:: rust

	// The newtype (compiler-checked type safety)
	struct UserId(i64); 
	struct LapseSecondsDuration(i64);
	
	let mut my_id  = UserId(15);
	let mut my_time = LapseSecondsDuration(53);
	
	//ERROR incompatible type
	my_id = my_time;
	
-------------
Unit Structs
-------------

-  These structs have no fields and are mostly used when you don't need to hold any value

.. code:: rust

	// A struct that represents an event that has occurred
	struct Shutdown;

	// How you use it:
	let system_shutdown = Shutdown;
	// ... can be used later to check the type

.. note:: These are often used for some more advanced topics like :rust:`Traits`