============================
Implementing Unsafe Traits
============================

----------------------------
Implementing Unsafe Traits
----------------------------

Like with functions, you can mark a trait as :rust:`unsafe` if the
implementation must guarantee particular conditions to avoid undefined
behaviour.

For example, the :rust:`zerocopy` crate has an unsafe trait that looks
:url:`something like this <https://docs.rs/zerocopy/latest/zerocopy/trait.IntoBytes.html>`:

.. code:: rust

   use std::{mem, slice};

   /// ...
   /// # Safety
   /// The type must have a defined representation and no padding.
   pub unsafe trait IntoBytes {
       fn as_bytes(&self) -> &[u8] {
           let len = mem::size_of_val(self);
           let slf: *const Self = self;
           unsafe { slice::from_raw_parts(slf.cast::<u8>(), len) }
       }
   }

   // SAFETY: `u32` has a defined representation and no padding.
   unsafe impl IntoBytes for u32 {}

.. raw:: html

---------
Details
---------

There should be a :rust:`# Safety` section on the Rustdoc for the trait
explaining the requirements for the trait to be safely implemented.

The actual safety section for :rust:`IntoBytes` is rather longer and more
complicated.

The built-in :rust:`Send` and :rust:`Sync` traits are unsafe.

.. raw:: html

