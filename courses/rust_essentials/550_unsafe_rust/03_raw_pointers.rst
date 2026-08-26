==============
Raw Pointers
==============

-------------------
Raw Pointer Types
-------------------

**Raw pointers provide low-level access without reference guarantees**

* :rust:`*const T` - raw pointer used to read a :rust:`T`
* :rust:`*mut T` - raw pointer that may write a :rust:`T`

**Characteristics of Raw Pointers**

* May alias other raw pointers
* May be null, dangling, or misaligned
* May point to uninitialized or invalid memory
* Do not own the pointed-to value

  * No automatic lifetime or cleanup management

-----------------------
Creating Raw Pointers
-----------------------

**Creating a raw pointer is safe and needs no unsafe block**

.. code:: rust

  let mut treasure = 5;

  // Raw pointers created from references
  let clue = &treasure as *const i32;
  let key = &mut treasure as *mut i32;

  // Raw pointer created from an arbitrary address
  let mystery_address = 0xdeadbeef_usize;
  let mystery_pointer = mystery_address as *const i32;

.. warning::

  Safe pointer creation does not make the pointer safe to dereference

----------------------------
Dereferencing Raw Pointers
----------------------------

**Dereferencing a raw pointer requires an unsafe block**

.. code:: rust

    let mut ellen_ripley = 0;
    let face_hugger = &raw mut ellen_ripley;

    // SAFETY: 'face_hugger' points to live, aligned,
    // initialized memory with exclusive access
    unsafe {
        *face_hugger = 426;
        println!("Ellen Ripley status: {}", *face_hugger);
    }

.. note::

  The safety comment explains why the dereference is valid

---------------------------------
Aliasing and Safety Obligations
---------------------------------

**Raw pointers may alias the same memory without a compilation error**

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
* Invalid pointers or aliasing may cause undefined behavior

.. warning::

  Rust permits these pointer operations, but their safety must be justified
