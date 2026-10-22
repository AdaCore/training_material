=======================
The Typestate Pattern
=======================

--------------------
What Is Typestate?
--------------------

* Typestate represents a value's logical state in its type

* Different states expose different methods

* A consuming transition returns a handle with a different state type

.. code:: rust

  Pin<Unconfigured>  // No input/output methods yet
  Pin<Input>         // Exposes is_high()
  Pin<Output>        // Exposes set_high()

-----------------------------
Runtime State vs. Typestate
-----------------------------

* **Runtime state**

  * A value stores its mode and checks it when an operation is called
  * Useful when the mode must vary dynamically
  * Misuse can be reported at runtime

* **Typestate**

  * The mode appears in the type
  * Invalid method calls fail to compile
  * No runtime mode field or check is needed to select these methods

.. note::

  Configuring hardware still requires runtime work

-------------------------------
Representing States with ZSTs
-------------------------------

* A zero-sized type (ZST) has a size of zero bytes

* Unit structs make simple state markers

.. code:: rust

  pub struct Unconfigured;
  pub struct Input;
  pub struct Output;

  assert_eq!(core::mem::size_of::<Output>(), 0);

* Each marker is a distinct type, even though it carries no data

-----------------------------------
Associating a State with a Handle
-----------------------------------

* The type parameter :rust:`Mode` identifies the pin's state

* :rust:`PhantomData<Mode>` uses it without storing a :rust:`Mode` value

.. code:: rust

  use core::marker::PhantomData;

  pub struct Pin<Mode> {
      number: u8,
      _mode: PhantomData<Mode>,
  }

* Private fields prevent callers from constructing arbitrary handles

* The marker adds no storage to the pin handle

----------------------------------
What Is Checked at Compile Time?
----------------------------------

* :rust:`Pin<Input>` and :rust:`Pin<Output>` are different types

* Rust checks whether a method exists for the current type

* :rust:`PhantomData` requires no runtime storage

* Hardware reconfiguration and I/O still happen at runtime

.. note::

  Typestate does not guarantee zero instructions or zero binary size
