===================
Safe Abstractions
===================

---------------------------------
Encapsulating Unsafe Operations
---------------------------------

**Safe abstractions provide a safe interface to unsafe operations**

* Callers use a safe interface
* Implementation handles unsafe operations

  * Keeps unsafe operations small and easy to audit

* If safety relies on caller guarantees

    * Expose an unsafe API
    * Document its :rust:`# Safety` contract

* Examples: :rust:`Vec<T>`, :rust:`String`, and many standard-library types

.. note::

  Callers need no :rust:`unsafe` block!

-------------------------------
Encapsulating the Unsafe Read
-------------------------------

**A safe method can perform an unsafe operation internally**

.. code:: rust

   #[repr(C)]
   union Avenger {
       banner: i8,
       hulk: u8,
   }

* Both fields share the same byte
* Both :rust:`i8` and :rust:`u8` accept every bit pattern

.. code:: rust

  impl Avenger {
       fn as_unsigned(&self) -> u8 {
            // SAFETY: Either field initializes the shared byte,
            // and every bit pattern is valid for 'u8'
            unsafe { self.hulk }
       }
  }

-------------------------
Calling the Safe Method
-------------------------

**Caller does not need an** :rust:`unsafe` **block**

.. code:: rust

  let avenger = Avenger { banner: -1 };
  println!("Avenger: {}", avenger.as_unsigned());

.. code:: output

  Avenger: 255

* Caller invokes a safe method
* Implementation guarantees safety of the operation

.. warning::

   This works for :rust:`i8` and :rust:`u8`, but not necessarily for other types

------------------
Unsound Safe API
------------------

**An unsound safe API can cause undefined behavior**

.. code:: rust

    // Safe callers can pass 'None'!
    fn unwrap_option(value: Option<i32>) -> i32 {
        unsafe { value.unwrap_unchecked() }
    }

* :rust:`unwrap_unchecked` requires :rust:`Some`
* Safe callers can pass :rust:`None`

.. note::

  **Unsound** means safe callers can trigger undefined behavior
