====================
Generics and Traits
====================

--------------
Trait Bounds
--------------

-  Generic data :rust:`<T>` is very broad 

-  Compiler will restrict what can be done with <T> 
   -  Doesn't know if number or string or anything else
   
-  Traits are the **fine print** on a generic contract

   -  Ensure the logic only executes on types that "fit" the requirements
   
-  Added in the :rust:`<>` next to the type

.. code:: rust

	fn print_it<T: std::fmt::Display>(item: T) {
		println!("{}", item); // Only works if T implements Display
	}

-  or with a :rust:`where` clause
   
.. code:: rust

	fn complex_function<T, U>(t: T, u: U) 
    where 
        T: Display, 
        U: Debug
    { ... }
	

------------------------
Adding Constraints
------------------------

Adding a trait to a generic object gives it more functionalities

.. code:: rust
	// Compiler: "What if T is a name? Is 'Bob' < 10?"
	fn is_small<T>(item: T) -> bool {
		item < 10 
	}

:error:`ERROR: "binary operation '<' cannot be applied to type 'T'"`	

.. code:: rust

	fn is_smaller<T: PartialOrd>(item: T, max_v: T) -> bool {
		item < max_v  
	}

------------------------
Satisfying Constraints
------------------------

-  Adding a trait restrict types that satisfy the generic contract

.. code:: rust

	struct Vegetable;

	fn is_smaller<T: PartialOrd>(item: T, threshold: T) -> bool {
		item < threshold  
	}

	fn main() {
		let potato : Vegetable;
		let sweet_potato : Vegetable;
		println!("{}", is_smaller(5, 10));      // Compares i32 to i32
		println!("{}", is_smaller(1.5, 10.0));  // Compares f64 to f64
		println!("{}", is_smaller(potato , sweet_potato));	 
	}

:error:`error[E0277]: can't compare 'vegetable' with 'vegetable'`

-----------------------
Trait Bounds Generics
-----------------------

-  User defined traits can be constraints for a generic function

.. code:: rust

    trait Speak {
        fn say_hello(&self);
    }
    
    struct Dog;
    
    impl Speak for Dog {
        fn say_hello(&self) {
            println!("Woof!");
        }
    }
    
    // This function ONLY accepts types that can 'Speak'
    fn make_it_speak<T: Speak>(item: T) {
        item.say_hello();
    }
    
    fn main() {
        let pet = Dog;
        make_it_speak(pet); // Output: Woof!
    }

----------------
Turbofish "::<>"
----------------

-  Usually Rust compiler determines the type to use from context

   -  Sometimes there is *ambiguity*
   
.. code:: rust

    // Error: "type annotations needed"
    // The compiler knows it's a Vec, but a Vec of what?
    let x = Vec::new();
	
-  The turbofish :rust:`::<>` syntax is used to dispell ambiguity

.. code:: rust

    // The Turbofish dispels the mystery!
    let x = Vec::<i32>::new();

-----------------
Multiple Traits
-----------------

-  Can have multiple trait bounds

   -  Using the :rust:`+` operator


.. code:: rust

	fn complex_function<T, U>(t: T, u: U) 
    where 
        T: Display + Clone, 
        U: Debug + PartialOrd 
    { ... }



----------------
Generic Traits
----------------

-  Traits can be made generic

   -  Allows the same trait to behave differently with each type

.. code:: rust

	// T is the "Target" type we want to turn into
	trait Transform<T> {
		fn convert(&self) -> T;
	}

	struct Minutes(i32);

	// Rule for converting Minutes to Seconds
	impl Transform<i32> for Minutes {
		fn convert(&self) -> i32 { self.0 * 60 }
	}

	// Rule for converting Minutes to a String
	impl Transform<String> for Minutes {
		fn convert(&self) -> String { format!("{} minutes", self.0) }
	
    


--------------
"derive" Macro and Generics
--------------

-  :rust:`derive` macro can be used on generic struct using standard traits

   -  Can't be used on generic traits
   
.. code:: rust
    
	// Compiler assumes T has 'Debug' trait
	#[derive(Debug)]
	struct Box<T> {
		content: T,

	struct Secret; // Note: No Debug here

	fn main() {
		let good_box = Box { content: 42 }; 
		println!("{:?}", good_box); // Works (i32 is Debug)

		let bad_box = Box { content: Secret };
		// ERROR: "Box<Secret> doesn't implement Debug"
		// Secret doesn't implement Debug, derive macro fails
	}

