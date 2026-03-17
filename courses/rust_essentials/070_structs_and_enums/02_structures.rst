=========
Structs
=========

--------
Basics
--------

-  :rust:`struct` creates a type that can hold multiple related values

   -  Visually similar to :cpp:`struct` in C/C++ or :ada:`record` in Ada

-  Can hold any type that is :dfn:`Sized`

   -  Size is known at compile time

-  Fields accessed via dot notation

-  Called :dfn:`named-field struct`
   

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
Nesting Structs
-----------------
   

.. code:: rust

    struct Engine {
        horsepower: u16,
        fuel_type: String,
    }
    struct Car {
        new: bool,
        // 'power_plant' is an 'Engine' struct inside 'Car' struct
        power_plant: Engine, 
    }
    
----------------------
Beware of Recursion!
----------------------
   
-  Structs **cannot** be recursive
   -   Type would not be **Sized**

.. code:: rust

    // 'RussianDoll' size = size of u8 + size of itself
    // Size is infinite
    struct RussianDoll {
        size: u8,
        // ...another 'RussianDoll'! (Infinite recursion)
        inner_doll: RussianDoll, 
    }
    
.. container:: latex_environment footnotesize

   :error:`error[E0072]: recursive type 'RussianDoll' has infinite size` 
  
-----------------------
Struct Initialization
-----------------------

-  No partial Initialization possible
   -  No implicit default values

.. code:: rust

    struct User {
        active: bool,
        sign_in_count: u64,
        logged_in: bool,
    }
    let user_1 : User;
    let user_2 = User {
        active: true,
        sign_in_count: 1,
        logged_in: true,
    };	
	let user_3 = User {
        active: true,
        sign_in_count: 1
    };

.. container:: latex_environment footnotesize
	
  :error:`error[E0063]: missing field 'logged_in' in initializer of 'User'`
    
--------------------------------
Field Initialization Shorthand
--------------------------------

-  If field and variable have same name, it could be written only once
   -  This is called :dfn:`Field Init Shorthand`
   -  Compiler automatically expands the variable
   -  *Name association* and *Field Init Shorthand* can be used together
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
      // Same as 'active: active'
      // Field takes value of local variable
      active,
      // Name association is still possible
      sign_in_count: sign_in_count,
      logged_in: true;
    };

------------------------
Struct Update Operator 
------------------------

-  Creation of :rust:`struct` based on another instance via '..' operator 
   -  Specify values only for the fields that need to change 
   -  Unspecified fields are *copied* or *moved* from the base instance
-  Base instance can't be followed by a comma
   -  Must be at the end of the declaration
   
.. warning:: Fields are *moved* if their type (e.g., :rust:`String`)  doesn't implement the :rust:`copy` trait  

.. code:: rust

    struct Settings {
        font_size: u8,
        active: bool,
    }
    let default_set = Settings {
        font_size: 14,
        active: false,
    };
    // Only change 'active' to true in 'set_1'
    let set_1 = Settings {
        active: true,   // Overridden field
        ..default_set   // Copy all other fields ('font_size')
    };
    let set_2 = Settings {
        ..default_set   // Copy all fields
    };


---------
Mutable
---------

-  Mutability applies to the entire instance
   -  No partial application for some fields
   
.. code:: rust

    struct CatStatus {
        energy_level: u8,
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

-  Like named-field :rust:`struct`, can hold any type that is **Sized**
   -  Useful to give a structure a specific name without naming any fields
-  First element of a tuple is 0 not 1  

.. code:: rust

    struct Character(
        u64,   // Power
        i64,   // Money
        bool,  // Is good?
    );
    // How you use it:
    let hero = Character(10000, -500, true);
    println!("Power level is : {}", hero.0);
    println!("Money is : {}", hero.1);
    
    println!("out of bound is : {}", hero.3);
    
:error:`error[E0609]: no field '3' on type 'Character'`

----------------
Value Illusion
----------------

-  Tuple struct type declaration defines both

   -  Data type
   
   -  Constructor function call
   
.. code:: rust
    
    struct Point(i32, i32);
    // Creates an alias
    let coord = Point;  // 'Point' is the constructor function
       
.. warning:: 
    
   :rust:`coord` is NOT a variable of type :rust:`Point`. It is an alias for the constructor function call

.. code:: rust 
    
    // Calls the alias for 'Point' constructor  
    // Initializes the tuple
    let maximum = coord(1,2); // 'maximum' is a 'Point'
    
-    This doesn't compile

.. code:: rust
    
    // Calls the 'Point' constructor 
    // No initilization because of missing fields
    let coord2 = Point();

.. container:: latex_environment footnotesize
    
   :error:`error[E0061]: this struct takes 2 arguments but 0 arguments were supplied`
   
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
Idiom: Newtype
----------------

-  A :dfn:`newtype` is a tuple :rust:`struct` with a single field 

   -  Used to ensure type safety

.. code:: rust

    struct Feet(i32);
    struct Inches(i32);

    let mut distance = Feet(12) + Inches(3);
	
:error:`error[E0369]: cannot add 'Inches' to 'Feet'`
