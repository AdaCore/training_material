========================================
What We Didn't Cover (Advanced Topics)
========================================

--------------------------------------
Advanced Tools and Low-Level Features
--------------------------------------

To explore Unsafe Rust more deeply, investigate

* **Miri**

  * Interpreter for Rust's Mid-level Intermediate Representation (MIR)
  * Helps detect many forms of undefined behavior in unsafe code

* **Inline Assembly**

  * :rust:`core::arch::asm!` embeds architecture-specific assembly

* **Undefined Behavior Deep Dive**

  * Aliasing rules
  * Uninitialized memory with :rust:`MaybeUninit<T>`
  * Layout guarantees such as :rust:`#[repr(C)]` and :rust:`#[repr(packed)]`

------------------------------------------
Manual Memory and Pointer Operations
------------------------------------------

* **Custom Allocators**

  * :rust:`std::alloc`
  * The :rust:`GlobalAlloc` trait
  * Manual heap allocation and deallocation

* **Advanced Pointer Arithmetic**

  * :rust:`ptr::read`
  * :rust:`ptr::write`
  * :rust:`ptr::copy_nonoverlapping`

.. warning::

  Mastering unsafe code requires deep knowledge of memory layout, compiler
  optimizations, and architecture constraints. Prefer Safe Rust abstractions
  whenever possible.
