========
Unions
========

--------------------
What Is a "union"?
--------------------

* Holds alternative representations of data in overlapping memory
  * Initialization writes exactly one field
  * Rust does not track an active field
    * Unlike :rust:`enum` that tracks which variant is active
* Storage fits the largest field, plus any padding

.. code:: rust

   #[repr(C)]
   union Avenger {
       banner: i8,
       hulk: u8,
   }

.. note::

  :rust:`#[repr(C)]` ensures all fields start at offset 0

----------------------
Writing Union Fields
----------------------

**Writing to a union field is safe**

.. code:: rust

   let mut avenger = Avenger { banner: -1 };

   // No unsafe block required
   avenger.hulk = 255;

* Writing another field overwrites shared storage
* Writing does not read or interpret previous field value

----------------------
Reading Union Fields
----------------------

**Reading is an unsafe operation**

.. code:: rust

   let avenger = Avenger { banner: -1 };

   // SAFETY: Both fields share an initialized byte,
   // and every bit pattern is valid for 'u8'
   let hulk = unsafe { avenger.hulk };

   println!("Hulk: {hulk}");

.. code:: output

   Hulk: 255

* Stored bits must be valid for selected field
* :rust:`-1_i8` and :rust:`255_u8` have the same bit pattern

.. note::

  Destructuring a union through pattern matching also requires :rust:`unsafe`

--------------------------
Reading Without "unsafe"
--------------------------

**Reading a union field without** :rust:`unsafe` **causes a compilation error**

.. code:: rust

    let avenger = Avenger { banner: -1 };
    let hulk = avenger.hulk; // This won't compile
    println!("Hulk: {hulk}");

.. container:: latex_environment footnotesize

  .. code:: error

    error[E0133]: access to union field is unsafe and requires unsafe block
