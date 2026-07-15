==============
Raw Pointers
==============

-------------------
Raw Pointer Types
-------------------

Raw pointers are similar to references, but the compiler does not enforce
all of the same safety guarantees

* :rust:`*const T` - raw pointer used for reading a :rust:`T`
* :rust:`*mut T` - raw pointer that may be used for writing a :rust:`T`

**Characteristics of Raw Pointers**

* May alias other raw pointers
* May be null, dangling, or misaligned
* May point to uninitialized or otherwise invalid memory
* Do not own the pointed-to value

  * No automatic lifetime management or cleanup

-----------------------
Creating Raw Pointers
-----------------------

Creating a raw pointer is safe and requires no unsafe block

.. code:: rust

  let mut num = 5;

  // Creating raw pointers from references
  let r1 = &num as *const i32;
  let r2 = &mut num as *mut i32;

  // Creating a raw pointer from an arbitrary address
  let address = 0x012345usize;
  let r3 = address as *const i32;

.. warning::

  Pointer creation being safe does not mean that the resulting pointer is
  safe to dereference.

----------------------------
Dereferencing Raw Pointers
----------------------------

Dereferencing requires an unsafe block

.. code:: rust

  fn main() {
      let mut num = 5;
      let r1 = &raw mut num;

      // SAFETY: `r1` points to the live, aligned, initialized local `num`,
      // and this block has exclusive access while writing through it.
      unsafe {
          *r1 = 10;
          println!("r1 is: {}", *r1);
      }
  }

The safety comment documents why the pointer is valid for the operations
performed inside the block.

-------------------------------
Aliasing and Safety Obligations
-------------------------------

Raw pointers can alias the same memory without a compiler error

.. code:: rust

  let mut num = 5;
  let r1 = &raw mut num;
  let r2 = r1; // Both raw pointers point to `num`

  // SAFETY: Both writes are sequential, `num` is live and aligned,
  // and no references or other threads access it during these writes.
  unsafe {
      *r1 = 10;
      *r2 = 20;
  }

  println!("num is: {num}");

* This single-threaded example performs sequential writes

  * It is **not** itself a data race

* Unsynchronized concurrent access, invalid pointers, or incompatible
  reference aliasing can cause undefined behavior

.. warning::

  The compiler permits the pointer operations; the safety argument is the
  programmer's responsibility.
