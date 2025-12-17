=========
Structs
=========

--------
Basics
--------

-  :rust:`struct` creates a type that can hold multiple related values

   -  Visually similar to a struct in C/C++, or a record in Ada

-  Can hold any type that is :dfn:`Sized`

   -  Size is known at compile time

-  Fields accessed via dot notation

-  Called **named-field struct**
   

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
		println!("You post too much!");
	}  

-----------------
Nesting structs
-----------------
   

.. code:: rust

	struct Engine {
		horsepower: u16,
		fuel_type: String,
	}
	struct Car {
		new: bool,
		// power_plant is a component of Car struct
		power_plant: Engine, 
	}

	
----------------------
Beware of Recursion!
----------------------
   
-  Can't be **recursive**
   -   Type would not be **not sized**

.. code:: rust

	// RussianDoll size is size of u8 + size of itself
	// Size is infinite
	struct RussianDoll {
		size: u8,
		// ...another RussianDoll!" (Infinite recursion)
		inner_doll: RussianDoll, 
	}
	
.. container:: latex_environment footnotesize

   :error:`error[E0072]: recursive type 'RussianDoll' has infinite size` 


	
-----------------------
Struct Initialization
-----------------------

-  Initialization of every field is **mandatory** 
-  No implicit default values

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
		logged_in: true;
	};

	
--------------------------------
Field Initialization Shorthand
--------------------------------

-  If a variable has the same name as field, name could be written only once
   -  Compiler automatically expands the variable
   -  Name association and :dfn:`Field Init Shorthand` can be used together
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
			logged_in: true;
	};


------------------------
Struct Update Operator 
------------------------

-  Creation of :rust:`struct` based on another instance via '..' operator 
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
		..default_set   // Copy all other fields (font_size)
	};
	let set_2 = Settings {
		..default_set   // Copy all fields
	};
	let set_3 = Settings { 
		..default_set,  // ERROR, can't be followed by a comma
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

-  Like Named-field :rust:`struct`, can hold any type that is **Sized**
   -  Useful to give a structure a specific name without naming any fields
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
	
	println!("out of bound is : {}", hero.3);
	
:error:`error[E0609]: no field `3` on type 'Character'`
	
-------------------------
Type Safety with Tuples
-------------------------

-  **Name** differentiates types 
   -  Not their definition
-  Tuple structs with the same definition are different types 
   

.. code:: rust

	struct Point(i32, i32);
	struct Size(i32, i32);

	let coordinates = Point(10, 20);
	let mut dimension = Size(30, 40);
	
	dimension = coordinates; // ERROR
	
:error:`error[E0308]: mismatched types`
	
	
	
----------------
Idiom: NewType
----------------

//JBE
-  A :dfn:`newtype` is a tuple :rust:`struct` with a single field. 

   -  Used to ensure type safety


.. code:: rust

	struct UserId(i64); 
	struct LapseSecondsDuration(i64);
	
	let mut my_id  = UserId(15);
	let mut my_time = Duration(53);

-  :rust:`UserId` and :rust:`Duration` are both :rust:`i64` but can't assign one to the other
	
	// ERROR mismatched types
	my_id = my_time;
//JBE	


