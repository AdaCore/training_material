==================
Unsafe Functions
==================

------------------
Safety Contracts
------------------

**An unsafe function transfers a safety obligation to its caller**

* :rust:`# Safety` is the conventional Rustdoc section for those requirements

.. code:: rust

  /// # Safety
  ///
  /// Callers must satisfy the documented requirements

* Safety contracts document requirements callers must uphold

------------------------------
Declaring an Unsafe Function
------------------------------

**Use an unsafe function when safety depends on caller guarantees**

* Declared with :rust:`unsafe fn`
* Caller must satisfy documented *safety contract*

.. code:: rust

  /// # Safety
  ///
  /// 'value' must be 'Some'
  unsafe fn get_some(value: Option<i32>) -> i32 {
      // SAFETY: Caller guarantees 'value' is 'Some'
      unsafe { value.unwrap_unchecked() }
  }

.. tip::

  Always use explicit :rust:`unsafe` blocks, even inside :rust:`unsafe` functions

----------------------------
Calling an Unsafe Function
----------------------------

**Calling an unsafe function requires an unsafe context**

.. code:: rust

  // SAFETY: The argument is 'Some', as required
  unsafe { get_some(Some(42)) };

.. note::

  * :rust:`# Safety` states what must be true
  * :rust:`// SAFETY:` explains why it is true here

-------------------------------
Unsafe Function Call Failures
-------------------------------

**An unsafe context does not guarantee a safe call**

  * Without an unsafe context, the compiler rejects the call

.. code:: rust

  get_some(Some(32)); // Missing unsafe context

.. code:: error

  error[E0133]: call to unsafe function `get_some` is unsafe
  and requires unsafe block

* An unsafe context does not verify the safety contract

.. code:: rust

  unsafe { get_some(None) }; // Undefined behavior
