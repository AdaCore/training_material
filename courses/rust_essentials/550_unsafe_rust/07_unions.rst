========
Unions
========

--------------------
What Is a "union"?
--------------------

A :rust:`union` stores different fields in shared storage

* Declared similarly to a :rust:`struct`, using the :rust:`union` keyword
* The fields share common storage
* The union is large enough to hold its largest field
* Initialization specifies exactly one field
* Rust does not track an active field

.. code:: rust

   union Avenger {
       banner: i8,
       hulk: u8,
   }

   let avenger = Avenger { banner: -1 };

.. note::

   Union fields cannot require automatic destruction; values such as :rust:`String` require :rust:`ManuallyDrop<T>`

----------------------
Writing Union Fields
----------------------

**Writing a union field is safe**

.. code:: rust

   union Avenger {
       banner: i8,
       hulk: u8,
   }

   let mut avenger = Avenger { banner: -1 };

   // No unsafe block required
   avenger.hulk = 255;

* Initialization writes the selected field
* Assigning to another field overwrites the same shared storage
* Writing does not read or interpret the previous field value
* No :rust:`unsafe` block is required for a field write

.. note::

   A union may be written through any of its fields without reading the previous contents

--------------------
Reading Union Fields
--------------------

Reading a union field requires :rust:`unsafe`

.. code:: rust

   union Avenger {
       banner: i8,
       hulk: u8,
   }

   let avenger = Avenger { banner: -1 };

   // SAFETY: Every bit pattern is valid for `u8`
   let hulk = unsafe { avenger.hulk };
   println!("Hulk: {hulk}");

.. code:: output

   Hulk: 255

* A read interprets the stored bits as the selected field type
* Rust does not check which field was written previously
* Pattern matching on a union field is also an unsafe read
* The stored bits must form a valid value for the field being read

.. warning::

   :rust:`banner: -1` and :rust:`hulk: 255` are two interpretations of the same eight bits
