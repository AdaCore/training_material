============================
Constraints and Properties
============================

--------------
Trait Bounds
--------------

-  Generic data :rust:`<T>` has almost no limitations

-  Compiler will restrict what can be done with :rust:`<T>` 
   -  Doesn't know if it can do math, order or anything else 
   
-  Traits are the **fine print** on a generic **contract**

   -  Ensure the logic only executes on types that "fit" the requirements
   
-  Added in the :rust:`<>` next to the type

.. code:: rust

  fn smaller<T: PartialOrd>(item: T, max_v: T) -> bool {
    item < max_v  
  }
  
  
------------------------
Meeting Constraints
------------------------

Adding a trait restricts types that satisfy the generic contract

.. code:: rust

  struct Vegetable;

  fn smaller<T: PartialOrd>(item: T, threshold: T) -> bool {
    item < threshold  
  }

  
  let potato : Vegetable;
  let sweet_potato : Vegetable;
  println!("{}", smaller(5, 10));      
  println!("{}", smaller(potato , sweet_potato));   
  

:error:`error[E0277]: can't compare 'Vegetable' with 'Vegetable'`
  

--------------------
Adding Constraints
--------------------

Adding a trait to generic specify what capabilities a type must have

.. code:: rust

  // Compiler: "What if 'T' is a string? Is 'Bob' < 10?"
  fn is_small<T>(item: T) -> bool {
    item < 10 
  }

.. container:: latex_environment scriptsize

  :error:`error[E0369]: binary operation '<' cannot be applied to type 'T'`  

.. code:: rust

  fn smaller<T: PartialOrd>(item: T, max_v: T) -> bool {
    item < max_v  
  }


------------------------------------
User-Defined Traits as Constraints
------------------------------------

-  Can be constraints for a generic function

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
    
  let pet = Dog;
  make_it_speak(pet);
    
  
:command:`Woof!`  

------------------
Turbofish "::<>"
------------------

-  Compiler determines the type to use from context

   -  Sometimes there is *ambiguity*
   
.. code:: rust
    
  // Vec<T> is a generic struct
  // Vec defines an associated function called 'new'
  // The compiler knows it's a Vec, but a Vec of what?
  let x = Vec::new();
  
:error:`error[E0282]: type annotations needed for 'Vec<_>'`
  
-  The turbofish :rust:`::<>` syntax is used to removes ambiguity

.. code:: rust

  // The Turbofish dispels the mystery!
  let x = Vec::<i32>::new();

-----------------
Multiple Traits
-----------------

-  Can have multiple trait bounds

   -  Requires the :rust:`+` operator

.. code:: rust   
   
    fn complex_fn<T: Display + Clone,
	              U: Debug + PartialOrd>(t: T, u: U) 
    {  }

-  :rust:`where` clause can be used for better visibility

.. code:: rust

  fn complex_fn<T, U>(t: T, u: U) 
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

  // 'T' is the "Target" type we want to turn into
  trait Transform<T> {
    fn change(&self) -> T;
  }
  struct Minutes(i32);
  // Rule for converting Minutes to Seconds
  impl Transform<i32> for Minutes {
    fn change(&self) -> i32 { self.0 * 60 }
  }
  // Rule for converting Minutes to a String
  impl Transform<String> for Minutes {
      fn change(&self) -> String { format!("{} min", self.0) }
  
-----------------------------
"derive" Macro and Generics
-----------------------------

-  :rust:`derive` macro can be used on generic struct using standard traits

   -  Can't be used on generic traits
   
.. code:: rust
    
  // Compiler assumes T has 'Debug' trait
  #[derive(Debug)]
  struct Box<T> {
    content: T,
  }
  struct Secret; // Note: No 'Debug' here

  
  let good_box = Box { content: 42 }; 
  println!("{:?}", good_box); // Works (i32 is Debug)

  let bad_box = Box { content: Secret };
  println!("{:?}", bad_box);
  // 'Secret' doesn't implement 'Debug', derive macro fails
  
:error:`error[E0277]: 'Secret' doesn't implement 'Debug'`  
  