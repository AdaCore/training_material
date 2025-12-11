=========
Structs
=========

------------
The Basics
------------

-  :rust:`struct` creates a type that can hold multiple related values

   -  Visually similar to a struct in C/C++, or a record in Ada

-  Can hold any type that is :dfn:`sized`

   -  its size is known at compile time

-  Fields accessed via dot notation
   

.. code:: rust

	struct User {
		age: u8,
		number_of_messages: u32,
	}
	
	let myself = User {
		age: 32,
		number_of_messages: 275,	
	}
	
	if myself.number_of_messages > 250 {
		println!("You post too much !");
	}  

------------------------
Nesting inside structs
------------------------
   
-  Can hold :rust:`struct`

.. code:: rust

	struct Engine {
		horsepower: u16,
		fuel_type: String,
	}
	struct Car {
		make: String,
		model: String,
		new: bool,
		// power_plant is a component of Car struct
		power_plant: Engine, 
	}

	
------------------------
Beware of Recursion!
------------------------
   
-  can't be **recursive**
   -   Would make it a **not sized** type

.. code:: rust

	// RussianDoll size is size of u8 + size of itself
	// Size is infinite
	struct RussianDoll {
		size: u8,
		// ...another RussianDoll!" (Infinite recursion)
		inner_doll: RussianDoll, 
	}	

:error:`error[E0072]: recursive type 'RussianDoll' has infinite size`	
	
-----------------------
Struct Initialization
-----------------------

-  Initialization of every field of a :rust:`struct` is **mandatory** when you instantiate it 
-  There are no implicit default values

.. code:: rust

	struct User {
		active: bool,
		sign_in_count: u64,
		logged_in: bool,
	}
	let status = true;
	let attempt_number = 1;
	
	let user_1 = User {
		active: status,
		sign_in_count: attempt_number,
		logged_in
	};

	
----------------------
Field Init Shorthand
----------------------

-  If a local variable has the same name as the :rust:`struct` field, name could be written only once
   -  Compiler automatically expands the variable
   -  name association and Field Init Shorthand can be used together
-  No positional association allowed

.. code:: rust

	struct User {
		active: bool,
		sign_in_count: u64,
		logged_in: bool,
	}
	let active = true;
	let sign_in_count = 1;
	
	let user_1 = User {
			// Same as active: active,
			// The field takes value of local variable
			active,
			// named association is still possible
			sign_in_count: sign_in_count,
			logged_in,
	};


------------------------
Struct Update Operator 
------------------------

-  Allows creation of :rust:`struct` based on another instance via '..' operator 
   -  Specify values only for the fields that needs to change 
   -  All unspecified fields are copied from the instance following the '..'
-  Base instance can't be followed by a comma
   -  Has to be at the end of the declaration

.. code:: rust

	struct Settings {
		font_size: u8,
		active: bool,
	}
	let default_set = Settings {
		font_size: 14,
		active: false,
	};
	// only change 'active' to true in 'set_1'
	let set_1 = Settings {
		active: true,   // Overridden field
		..default_set // Copy all other fields (font_size)
	};
	let set_2 = Settings {
		..default_set // Copy all fields
	};
	let set_3 = Settings { 
		..default_set, // ERROR, can't be follow by a comma
	};
	
:error:`error: cannot use a comma after the base struct`

---------
Mutable
---------

-  Mutability applies to the entire instance
   -  No partial application for some fields
   
.. code:: rust

	struct CatStatus {
		energy_level: u8, // e.g., 100 for high energy
		is_napping: bool,
	}
	let mut active_cat = CatStatus {
        energy_level: 80,
        is_napping: false,
    };
	active_cat.is_napping = true;
	// ERROR
	let new_cat = CatStatus {
        mut energy_level: 80,
        is_napping: false,
    };
	

:error:`error: expected identifier, found keyword 'mut'`

---------------
Tuple Structs
---------------

-  Like regular :rust:`struct`, can hold any type that is **sized**
   -  Useful to give a structure a specific type name without naming all of its fields
-  First element of a tuple is 0 not 1  

.. code:: rust

	struct Character(
    u64,   // Power
	i64,   // Money
	bool,  // is good?
	);

	// How you use it:
	let hero = Character(10000, -500, true);
	println!("Power level is : {}", hero.0);
	println!("Money is : {}", hero.1);
	
-------------------------
Type Safety with Tuples
-------------------------

-  Name differentiates types not their definition,
   -  Tuple structs with the same definition are different types 
   

.. code:: rust

	struct Point(i32, i32);
	struct Size(i32, i32);

	let p = Point(10, 20);
	let mut s = Size(30, 40);
	
	s = p; // ERROR
	
:error:`error[E0308]: mismatched types`
	
	
	
----------------
Idiom: NewType
----------------


-  A :dfn:`newtype` is a tuple :rust:`struct` with a single field. 

   -  Used to ensure type safety

   -  :rust:`UserId` and :rust:`Duration` are both :rust:`i64` but can't assign one to the other

.. code:: rust

	// The newtype (compiler-checked type safety)
	struct UserId(i64); 
	struct LapseSecondsDuration(i64);
	
	let mut my_id  = UserId(15);
	let mut my_time = Duration(53);
	
	//ERROR incompatible type
	my_id = my_time;
	


