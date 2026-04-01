=====================
Interior Mutability
=====================

-----------------------------
Limitations of Strict Rules
-----------------------------

- Immutable references strictly **forbid** data modification
- Certain patterns require updating hidden state
  - During a read-only :rust:`&self` method call
- Compile-time borrow checks are traded for runtime checks

.. note::

   :dfn:`Interior mutability` enables safe modification through shared references

----------
"Cell<T>"
----------

- Guarantees safe mutation through a shared, read-only reference
- Designed for types that implement the :rust:`Copy trait`
  - Such as integers or booleans
- References to the inner data are never exposed

-------------------
"Cell<T>" Example
-------------------

**Values are exclusively copied in** (:rust:`set`) **and out** (:rust:`get`)

.. code:: rust

   use std::cell::Cell;

   struct Sensor {
      data: i32,
      read_count: Cell<u32>, // Can be modified even if 'Sensor' is '&self'
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

:output:`Sensor read 1 time(s).`

--------------
"RefCell<T>"
--------------

- Used for complex types, like :rust:`Vec` or :rust:`String` 
  - Where copying isn't cheap or possible
- Enforces borrowing rules at runtime rather than compile-time
- :rust:`.borrow()` for a reader
- :rust:`.borrow_mut()` for a writer 

.. code::rust

   use std::cell::RefCell;

   let memory = RefCell::new(vec![42, 10]);

   // Modify the vector through an immutable variable 'memory'
   let mut writer = memory.borrow_mut(); 
   writer.push(99);
      
   let reader = memory.borrow();  // This will panic

.. warning::

   If rules are violated, program will immediately panic and crash
