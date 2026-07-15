===============
Unsafe Traits
===============

---------------
Unsafe Traits
---------------

A trait is declared unsafe when an incorrect implementation could allow
Safe Rust to cause undefined behavior

* Declared using :rust:`unsafe trait`
* Implemented using :rust:`unsafe impl`
* The implementation is a promise that all required invariants are upheld

.. code:: rust

  /// # Safety
  ///
  /// Implementors must uphold the invariants documented by this trait.
  unsafe trait Foo {
      // Trait items go here
  }

  struct Bar;

  // SAFETY: `Bar` upholds every invariant required by `Foo`.
  unsafe impl Foo for Bar {}

-----------------
Send and Sync
-----------------

Common unsafe traits in the standard library include

* :rust:`Send`

  * The type can be transferred safely to another thread

* :rust:`Sync`

  * Shared references to the type can be used safely from multiple threads

.. warning::

  An incorrect manual implementation of :rust:`Send` or :rust:`Sync` can make
  otherwise Safe Rust unsound.
