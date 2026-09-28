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
  
.. code:: output

  Hello, 007!
  
-----------------------------
Coercing Types With "Deref"
-----------------------------

- Deref coercion converts :rust:`&T` to :rust:`&U`

  - When :rust:`T: Deref<Target = U>`

- Performs multiple "steps" of coercion at compile time

  - Zero runtime performance penalty

- Accesses inner value of smart pointers transparently

.. code:: rust 

  fn hello(name: &str) {
    println!("Hello, {name}!");
  }

  let my_box = Box::new(String::from("Rust"));

  hello(&my_box); 
  
.. code:: output

  Hello, Rust!
  
.. note::
  
  - :rust:`&my_box` is :rust:`&Box<String>`
  
  - Compiler coerces: :rust:`&Box<String>` -> :rust:`&String` -> :rust:`&str` 
  
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

  fn say(name: &str) { println!("Hello, {name}!"); }
  fn yell(name: &mut str) { name.make_ascii_uppercase(); }

  let aya = Box::new(String::from("Aya"));
  let mut zoe = Box::new(String::from("Zoé"));

.. list-table::
   :header-rows: 1
   :widths: 24 32 44

   * - **Call**
     - **Coercion**
     - **Trait / Result**
   * - :rust:`say(&aya)`
     - :rust:`&T` to :rust:`&U`
     - :rust:`Deref`
   * - :rust:`yell(&aya)`
     - :rust:`&T` to :rust:`&mut U`
     - :error:`E0308: mismatched types`
   * - :rust:`yell(&mut zoe)`
     - :rust:`&mut T` to :rust:`&mut U`
     - :rust:`DerefMut`
   * - :rust:`say(&mut zoe)`
     - :rust:`&mut T` to :rust:`&U`
     - :rust:`Deref`



