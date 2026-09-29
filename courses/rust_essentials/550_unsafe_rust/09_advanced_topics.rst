======================
What We Didn't Cover
======================

-------------------------
Topics for Deeper Study
-------------------------

* **Miri (tool/interpreter)**

  * Interprets Rust's Mid-level Intermediate Representation (MIR)
  * Detects many forms of undefined behavior in unsafe code

* **Inline Assembly**

  * :rust:`core::arch::asm!` embeds architecture-specific assembly

* **Undefined Behavior Deep Dive**

  * Aliasing rules
  * Uninitialized memory with :rust:`MaybeUninit<T>`
  * Further layout guarantees such as :rust:`#[repr(C)]` and :rust:`#[repr(packed)]`

-----------------------------
Concurrency and Unsafe Rust
-----------------------------

**This module does not cover concurrency or thread-safety proofs**

* Data races and cross-thread aliasing
* The unsafe auto traits :rust:`Send` and :rust:`Sync`
* Manual :rust:`unsafe impl` of thread-safety guarantees
* Atomics, locks, and critical sections
* Safe access to shared mutable global state

.. warning::

  Bad concurrency guarantees can let Safe Rust trigger undefined behavior

--------------------------------------
Manual Memory and Pointer Operations
--------------------------------------

* **Custom Allocators**

  * :rust:`std::alloc`
  * :rust:`GlobalAlloc`
  * Manual heap allocation and deallocation

* **Advanced Pointer Operations**

  * :rust:`ptr::read`
  * :rust:`ptr::write`
  * :rust:`ptr::copy_nonoverlapping`

.. warning::

  Unsafe code may depend on exact layout, target, and optimization details
