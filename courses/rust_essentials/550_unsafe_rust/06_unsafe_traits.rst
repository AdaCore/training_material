===============
Unsafe Traits
===============

------------------------
When a Trait Is Unsafe
------------------------

**Unsafe traits define requirements the compiler cannot verify**

* Declared with :rust:`unsafe trait`
* Defines a safety contract for every implementation
* Violating the contract can cause undefined behavior in Safe Rust

.. code:: rust

  /// # Safety
  ///
  /// Implementors must uphold this trait's safety requirements
  unsafe trait Foo {
      // Trait items go here
  }

* Trait methods need not themselves be unsafe

------------------------------
Implementing an Unsafe Trait
------------------------------

**Implementing an unsafe trait requires an explicit promise**

* Use :rust:`unsafe impl`
* Verify that every safety requirement is satisfied
* Document why the implementation satisfies the contract
* Normal type and syntax checks still apply

.. code:: rust

  struct Bar;

  // SAFETY: 'Bar' satisfies 'Foo' safety requirements
  unsafe impl Foo for Bar {}

.. warning::

  :rust:`unsafe impl` promises the contract; it does not prove correctness
