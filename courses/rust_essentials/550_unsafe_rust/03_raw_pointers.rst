==============
Raw Pointers
==============

-------------------
Raw Pointer Types
-------------------

* Raw pointers provide low-level access *without* reference guarantees
  * :rust:`*const T` - raw pointer for reading
  * :rust:`*mut T` - raw pointer for reading and writing
* Characteristics of a raw pointer
  * May alias other raw pointers
  * May be null, dangling, or misaligned
  * May point to uninitialized or invalid memory
  * Does not own pointed-to value
    * Keeping the pointer does not keep the value alive
    * Dropping the pointer does not destroy the value

.. warning::

  Pointer types do not guarantee valid memory accesses

----------------------
Raw Pointer Creation
----------------------

**Creating a raw pointer is safe and needs no** :rust:`unsafe` **block**

.. code:: rust

  let mut treasure = 5;

  let clue = &raw const treasure;
  let key = &raw mut treasure;

* :rust:`&raw const` creates a :rust:`*const T`
* :rust:`&raw mut` creates a :rust:`*mut T`

.. warning::

  Just because it's safe to create doesn't make it safe to dereference!

--------------------------------------
Creating Raw Pointers via Conversion
--------------------------------------


.. code:: rust

  let treasure = 5;

- Cast from a reference

.. code:: rust

  let reference = &treasure;
  let clue = reference as *const i32;

- Convert an arbitrary address

.. code:: rust

  let mystery_address = 0xdeadbeef_usize;
  let mystery_pointer = mystery_address as *const i32;

.. warning::

  Creating this pointer is safe; accessing memory through it may not be

----------------------------
Dereferencing Raw Pointers
----------------------------

**Dereferencing requires an unsafe block**

.. code:: rust

    let mut ellen_ripley = 0;
    let face_hugger = &raw mut ellen_ripley;

    // SAFETY: 'face_hugger' points to live, aligned,
    // initialized memory with exclusive access
    unsafe {
        *face_hugger = 426;
        println!("Ripley status: {}", *face_hugger);
    }

.. note::

    :rust:`// SAFETY:` explains the reasoning to readers; recommended, not required

---------------------------------
Aliasing and Safety Obligations
---------------------------------

**Raw pointers may alias the same memory without compile error**

.. code:: rust

  let mut coins = 0;
  let mario = &raw mut coins;
  let luigi = mario; // Both point to 'coins'

  // SAFETY: 'coins' is live, aligned, and initialized
  // Writes are sequential, with no reference access
  unsafe {
      *mario = 64;
      *luigi = 120;
  }

* Raw-pointer aliasing is not checked
* Every access must remain valid and compatible
* Invalid or incompatible memory accesses may cause undefined behavior
