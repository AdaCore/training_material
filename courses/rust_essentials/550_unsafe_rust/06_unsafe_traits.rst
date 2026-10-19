===============
Unsafe Traits
===============

------------------------
What Are Unsafe Traits
------------------------

**Unsafe traits define requirements the compiler cannot verify**

* Declared with :rust:`unsafe trait`
* Implementors must satisfy the safety contract
* Violating the contract can cause undefined behavior

.. code:: rust

  /// # Safety
  /// All-zero bits must represent a valid value
  unsafe trait Zeroable {}

* Trait methods *don't need* to be unsafe

------------------------------
Implementing an Unsafe Trait
------------------------------

**Implementing an unsafe trait **

* Requires an explicit promise
  * Use :rust:`unsafe impl`
  * Verify that every safety requirement is satisfied
  * Document why the implementation satisfies the contract
* Normal type and syntax checks still apply

.. code:: rust

  // SAFETY: All-zero bits represent a valid u32
  unsafe impl Zeroable for u32 {}

.. warning::

  :rust:`unsafe impl` promises the contract; compiler doesn't verify correctness

-------------------------------
  Violating a Safety Contract
-------------------------------

**An unsafe implementation can violate its safety contract**

.. code:: rust

  // SAFETY: All-zero bits represent a valid &u32
  unsafe impl Zeroable for &u32 {}

* This compiles, but the **safety claim is false**
  * References cannot be null
* Code relying on this false guarantee may cause undefined behavior

.. warning::

  .. raw:: latex

    {\footnotesize

  :rust:`unsafe impl` does not guarantee correctness; the implementor is responsible

  .. raw:: latex

    }
