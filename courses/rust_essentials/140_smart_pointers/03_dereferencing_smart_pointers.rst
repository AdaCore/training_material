==============================
Dereferencing Smart Pointers
==============================

---------------
"Deref" Trait
---------------

- Types implementing :rust:`Deref` behave like references

- Returns a reference to the inner data

  - Using dereference operator :rust:`*`
  
  - Avoids moving ownership
  
- Dereference operator :rust:`*` calls on the :rust:`deref()` method

.. code:: rust 

  fn say_hello(name: i32) {
    println!("Hello, 00{name}!");
  }

  let agent = Box::new(7);
    
  say_hello(*agent); // No need to do 'hello(agent.0)'

  
------------------
"Deref" Coercion
------------------

- Converts reference to a type into reference to a different type

  - When needed if :rust:`Deref` is implemented between these types

- Performs multiple "steps" of coercion at compile time

  - Incurs zero runtime performance penalty

- Accesses inner value of smart pointers transparently

.. code:: rust 

  fn hello(name: &str) {
    println!("Hello, {name}!");
  }

  let my_box = Box::new(String::from("Rust"));
    
  // '&my_box' is '&MyBox<String>'
  // Rust coerces: '&Box<String>' -> '&String' -> '&str'
  hello(&my_box); 
  
------------  
"DerefMut"
------------  

- *subtrait* of Deref

  - Deref has to be implemented first
  
- Allows *mutable reference*

  - :rust:`Box<T>` implements :rust:`DerefMut`

.. code:: rust 

  use std::ops::{Deref, DerefMut};

  impl<T> Deref for MyBox<T> {
    type Target = T;
    fn deref(&self) -> &T { &self.0 }
  }

  impl<T> DerefMut for MyBox<T> {
    // Note: It uses the 'Target' defined in the Deref implementation
    fn deref_mut(&mut self) -> &mut T { &mut self.0 }
  }

-------------------------
Mutability and Coercion
-------------------------

**Prohibit &T to &mut T - never coerce** *immutable* **to** *mutable*

.. list-table::
   :header-rows: 1

   * - **From**
     - **To**
	 - **Trait Required**
     - **Result**

   * - :rust:`&T`
     - :rust:`&U`
	 - :rust:`T: Deref<Target = U>`
     - :color-green:`V` 

   * - :rust:`&mut T`
     - :rust:`&mut U`
	 - :rust:`T: DerefMut<Target = U>`
     - :color-green:`V`

   * - :rust:`&mut T`
     - :rust:`&U`
	 - :rust:`T: Deref<Target = U>`
     - :color-green:`V`

   * - :rust:`&T`
     - :rust:`&mut U`
	 - :color-red:`X`
     - :color-red:`X`


