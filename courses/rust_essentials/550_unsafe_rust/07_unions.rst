========
Unions
========

--------------------
What Is a "union"?
--------------------

**A union stores different fields in shared storage**

* Fields share the same storage
* The union is large enough for its largest field
* Initialization writes exactly one field
* Rust does not track an active field

.. code:: rust

   #[repr(C)]
   union Avenger {
       banner: i8,
       hulk: u8,
   }

* :rust:`#[repr(C)]` guarantees that every field starts at byte offset zero
* Fields that need destruction use :rust:`ManuallyDrop<T>`

----------------------
Writing Union Fields
----------------------

**Writing to a union field is safe**

.. code:: rust

   let mut avenger = Avenger { banner: -1 };

   // No unsafe block required
   avenger.hulk = 255;

* Writing another field overwrites the shared storage
* Writing does not read or interpret the previous field value

----------------------
Reading Union Fields
----------------------

**Reading from a union field is an unsafe operation**

.. code:: rust

   let avenger = Avenger { banner: -1 };

   // SAFETY: Both fields start at offset 0
   // Every bit pattern is valid for u8
   let hulk = unsafe { avenger.hulk };

   println!("Hulk: {hulk}");

.. code:: output

   Hulk: 255

* Rust does not track the last written field
* Union reads, including pattern matching, are unsafe
* Stored bits must be valid for the selected field
* :rust:`-1_i8` and :rust:`255_u8` are the same eight bits
