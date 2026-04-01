==============================
Dereferencing Smart Pointers
==============================

---------------
"Deref" Trait
---------------

- *Smart pointers* behave like references

  - because they implement :rust:`Deref`

- :rust:`Deref` returns a reference to the inner data

  - Calls on the :rust:`deref()` method

  - Using dereference operator :rust:`*`
  
  - Avoids moving ownership

.. code:: rust 

  fn say_hello(name: i32) {
    println!("Hello, 00{name}!");
  }

  let agent = Box::new(7_i32);
    
  say_hello(*agent); 
  
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

  hello(&my_box); 
  
.. note::
    
  '&my_box' is '&MyBox<String>'
  Rust coerces: '&Box<String>' -> '&String' -> '&str'  
  
------------  
"DerefMut"
------------  

- *Subtrait* of :rust:`Deref`

  - :rust:`Deref` has to be implemented first
  
- Allows *mutable reference*

  - :rust:`Box<T>` implements :rust:`DerefMut`

.. code:: rust 

  // 'my_box' is mutable and 'Box' implements DerefMut
  let mut my_box = Box::new(0);
  *my_box = 10; // 'DerefMut' is used

-------------------------
Mutability and Coercion
-------------------------

.. list-table::
   :header-rows: 1

   * - **From**
     - **To**
	 - **Trait Required**
     - **Result**

   * - :rust:`&T`
     - :rust:`&U`
	 - :rust:`T: Deref<Target = U>`
     - :math:`\textcolor{green!65!black}{\checkmark}`

   * - :rust:`&mut T`
     - :rust:`&mut U`
	 - :rust:`T: DerefMut<Target = U>`
     - :math:`\textcolor{green!65!black}{\checkmark}`

   * - :rust:`&mut T`
     - :rust:`&U`
	 - :rust:`T: Deref<Target = U>`
     - :math:`\textcolor{green!65!black}{\checkmark}`

   * - :rust:`&T`
     - :rust:`&mut U`
	 - :color-red:`X`
     - :color-red:`X`

.. note::

  Prohibit &T to &mut T - never coerce *immutable* to *mutable*
  
.. code:: rust

  fn hello(name: &str) {
    println!("Hello, {name}!");
  } 
  
  fn edit(name: &mut str) { println!("Hello, {name}!")  }

  let my_box = Box::new(String::from("Rust"));
  edit(&my_box);
  
:error:`error[E0308]: mismatched types`

.. code:: rust  
  
  let mut my_box = Box::new(String::from("Rust"));
  edit(&mut my_box);
  hello(&mut my_box);  



