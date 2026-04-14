===============
Dereferencing
===============

---------------
"Deref" Trait
---------------

- Smart pointers behave like references

  - Because they implement :rust:`Deref`

- :rust:`Deref` returns a reference to the inner data

  - Data is accessed with dereference operator :rust:`*`
  
  - Avoids moving ownership

.. code:: rust 

  fn say_hello(name: i32) {
    println!("Hello, 00{name}!");
  }

  let agent = Box::new(7_i32);
    
  say_hello(*agent); 
  
:command:`Hello, 007!`
  
-----------------------------
Coercing Types With "Deref"
-----------------------------

- Coercion allows conversion of a referenced type to a different type

  - If :rust:`Deref` is implemented between the types

- Performs multiple "steps" of coercion at compile time

  - Zero runtime performance penalty

- Accesses inner value of smart pointers transparently

.. code:: rust 

  fn hello(name: &str) {
    println!("Hello, {name}!");
  }

  let my_box = Box::new(String::from("Rust"));

  hello(&my_box); 
  
.. note::
  
  - &my_box is &MyBox<String>
  
  - Compiler coerces: &Box<String> -> &String -> &str 
  
------------  
"DerefMut"
------------  

- *Subtrait* of :rust:`Deref`

  - :rust:`Deref` must be implemented first
  
- Allows *mutable reference*

.. code:: rust 

  // 'my_box' is mutable and 'Box' implements 'DerefMut'
  let mut my_box = Box::new(0);
  *my_box = 10; // 'DerefMut' is used

-------------------------
Mutability and Coercion
-------------------------
  
.. code:: rust

  fn hello(name: &str) {
    println!("Hello, {name}!");
  } 
  
  fn edit(name: &mut str) { println!("Hello, {name}!")  }
  
- From &T to &U 
  - Trait required :rust:`T: Deref<Target = U>`

.. code:: rust

  let my_box = Box::new(String::from("Rust"));
  hello(&my_box);

- From &T to &mut U 
  - Not allowed

.. code:: rust

  edit(&my_box);
  
:error:`error[E0308]: mismatched types`

.. code:: rust  
  
  let mut my_box2 = Box::new(String::from("Rust"));
  
- From &mut T to &mut U 
  - Trait required :rust:`T: DerefMut<Target = U>`

.. code:: rust  
  
  edit(&mut my_box2);
  
- From &mut T to &U 
  - Trait required :rust:`T: Deref<Target = U>`

.. code:: rust  
  
  hello(&mut my_box2);  



