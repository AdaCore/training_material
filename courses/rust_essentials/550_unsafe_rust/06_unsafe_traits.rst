===============
Unsafe Traits
===============

------------------------
When a Trait Is Unsafe
------------------------

**Declare a trait unsafe when Safe Rust may rely on invariants that the compiler cannot verify**

* Declared with :rust:`unsafe trait`
* Defines a safety contract for every implementation
* An incorrect implementation can make Safe Rust unsound

.. code:: rust

  /// # Safety
  ///
  /// Implementors must uphold this trait's invariants
  unsafe trait Foo {
      // Trait items go here
  }

.. note::

  An unsafe trait defines an implementation contract; its methods need not be unsafe

------------------------------
Implementing an Unsafe Trait
------------------------------

**Implementing an unsafe trait requires an explicit promise**

* Use :rust:`unsafe impl`
* Verify every required invariant
* Document why the implementation satisfies the contract
* Normal type and syntax checks still apply

.. code:: rust

  struct Bar;

  // SAFETY: 'Bar' upholds every invariant required by 'Foo'
  unsafe impl Foo for Bar {}

.. warning::

  :rust:`unsafe impl` records responsibility for safety requirements; it does not prove correctness
