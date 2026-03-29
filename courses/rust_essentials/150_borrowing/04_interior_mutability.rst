=====================
Interior Mutability
=====================

-------------------------------
The Problem with Strict Rules
-------------------------------

- Immutable references strictly forbid data modification
- Certain patterns require updating hidden state
  - During a read-only :rust:`&self` method call
    - Like :rust:`read_count` on :rust:`Sensor` 
- Compile-time borrow checks are traded for runtime checks

.. note::

   **Interior mutability** enables safe mutation through shared references

----------
"Cell<T>"
----------

- :rust:`Cell<T>` is designed for types that implement the :rust:`Copy trait`
  - Such as integers or booleans
- References to the inner data are never exposed
- Values are exclusively copied in (:rust:`set`) and out (:rust:`get`)
  - Guarantees safe mutation through a shared, read-only reference

.. code:: rust

   use std::cell::Cell;

   struct Sensor {
      data: i32,
      read_count: Cell<u32>, // Can be mutated even if Sensor is &self
   }

   impl Sensor {
      fn read(&self) -> i32 {
         self.read_count.set(self.read_count.get() + 1);
         self.data
      }
   }

   let scanner = Sensor { data: 42, read_count: Cell::new(0) };
      
   scanner.read(); // Borrows immutably
   println!("Sensor read {} time(s).", scanner.read_count.get());

--------------
"RefCell<T>"
--------------

- Used for complex types, like :rust:`Vec` or :rust:`String` 
  - Where copying isn't cheap or possible
- Enforces the borrowing rules at runtime rather than compile-time
- Use :rust:`.borrow()` for a reader and :rust:`.borrow_mut()` for a writer 

.. code::rust

   use std::cell::RefCell;

   let memory = RefCell::new(vec![42, 10]);

   // Mutate the vector through an immutable variable 'memory'
   let mut writer = memory.borrow_mut(); 
   writer.push(99);
      
   let reader = memory.borrow();  // This will panic

.. warning::

   If the rules are violated, the program will immediately panic and crash
