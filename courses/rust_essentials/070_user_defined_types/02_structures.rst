============
Structs
============

------------
The Basics
------------

-  :rust:`struct` creates a type that can hold multiple related values

-  Similar to a struct in C, or a record in Ada

-  Can hold any type that is :dfn:'sized'

   -  means its size is known at compile time

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

	
------------------------
Beware of Recursivity !
------------------------
   
-  It can't be **recursive**
   -   Would make it an **unsized** type

.. code:: rust

	struct Car {
		make: String,
		model: String,
		new: bool,
	}
	// Buyer size is size of car + size of itself
	// Size is infinite
	struct Buyer {
		curr_vehicle: Car,
		// This is recursive
		previous_owner: Buyer, 
	}	

:error:`error[E0072]: recursive type 'Buyer' has infinite size`	
	
-----------------------
Struct Initialization
-----------------------

-  Initialization of every field of a :rust:`struct` is **mandatory** when you instantiate it 
-  There are no implicit default values

.. code:: rust

	struct User {
		email : String,    
		username : String, 
		active: bool,
		sign_in_count: u64,
	}
	fn send (text: String, new_usr: String) -> User {
		User {
			email : text,
			username: new_usr,
			active: true,
			sign_in_count: 1,
		}
	}
	
----------------------
Field Init Shorthand
----------------------

-  If a local variable has the same name as the :rust:`struct` field, name can be written only once
   -  Compiler automatically expands email, to email: email, because of the local variable

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
		theme_number: u16,
		font_size: u8,
		active: bool,
	}
	let base_settings = UserSettings {
		theme_number: 256,
		font_size: 14,
		active: false,
	};
	// only change 'active' to true in 'new_settings'
	let new_settings = UserSettings {
		active: true, // Overridden field
		..base_settings // Copy all other fields (theme_number, font_size)
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

-  These structs have no fields and are mostly used when you don't need to hold any value like states or events

.. code:: rust

	// A struct that represents an event that has occurred
	struct Shutdown;

	// How you use it:
	let system_shutdown = Shutdown;
	// ... can be used later to check the type

