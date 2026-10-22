=======================
Typestate in Practice
=======================

--------------------------------------
Portable Drivers with "embedded-hal"
--------------------------------------

* :rust:`embedded-hal` 1.x defines traits for common digital operations

  * A HAL can implement them for suitable pin configurations

* A portable driver can depend on a trait instead of a concrete pin type

.. code:: rust

  use embedded_hal::digital::OutputPin;

  fn set_led_high<P: OutputPin>(
      pin: &mut P,
  ) -> Result<(), P::Error> {
      pin.set_high()
  }

-------------------------------------
The Teaching API and "embedded-hal"
-------------------------------------

* Our earlier :rust:`Pin` methods are simplified inherent methods

  * :rust:`is_high(&self)` returns :rust:`bool`

* :rust:`embedded-hal` 1.x defines :rust:`InputPin::is_high()`

  * Takes :rust:`&mut self`
  * Returns :rust:`Result<bool, Self::Error>`

* A HAL chooses which configured types implement these traits

.. note::

  Not every HAL uses typestate in the same way

---------------------------------
Other Peripheral State Machines
---------------------------------

* Typestate can represent UART, SPI, or I2C configuration stages

* A builder may consume configuration values and return a ready peripheral

* Distinct types can also represent alternate-function pin routing

.. code:: rust

  // Illustrative marker, not a complete routing API
  pub struct Alternate<Function>(PhantomData<Function>);
  pub struct UartTx;

.. note::

  Valid routing still depends on the device's pins and alternate functions

-------------------------------
Pin Identity and Type Erasure
-------------------------------

* Some HALs include pin identity in the type: :rust:`Pin<PA5, Output>`

* An array requires all its elements to have the same type

* Erasing pin identity can make collections easier to build

.. code:: rust

  // Illustrative HAL API
  let first: ErasedPin<Output> = pa5.into_output().erase();
  let second: ErasedPin<Output> = pb2.into_output().erase();
  let pins = [first, second];

.. note::

  This extension preserves :rust:`Output` while erasing pin identity

------------
Trade-offs
------------

* **Benefits**

  * State-specific APIs and earlier diagnostics
  * Fewer runtime mode checks

* **Costs**

  * Additional types and generic signatures
  * Potentially longer compile times

* **Limits**

  * Dynamic reconfiguration, shared registers, and external changes
  * Other mechanisms may still be needed

.. tip::

  Typestate is a design tool, not a requirement for every peripheral API
