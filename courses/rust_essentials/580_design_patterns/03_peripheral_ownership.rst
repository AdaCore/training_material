======================
Peripheral Ownership
======================

------------------------------
Acquiring Peripheral Handles
------------------------------

* A PAC commonly groups device handles in a :rust:`Peripherals` struct

* :rust:`Peripherals::take()` returns :rust:`Option<Peripherals>` for one-time acquisition

.. code:: rust

  use stm32f4::stm32f401 as pac;

  fn initialize() {
      let dp = pac::Peripherals::take().unwrap();
      let _gpioa = dp.GPIOA;
      let _usart1 = dp.USART1;
  }

--------------------------
Two Different Guarantees
--------------------------

* **Runtime acquisition**

  * :rust:`take()` checks whether the handles have already been claimed
  * The first successful call returns :rust:`Some(...)`
  * Later calls return :rust:`None`
  * Calling :rust:`unwrap()` or :rust:`expect()` on :rust:`None` panics

* **Compile-time ownership**

  * Move and borrowing rules govern use of an acquired handle

.. note::

  The implementation of :rust:`take()` need not use an atomic flag

-----------------------------------
Moving a Peripheral into a Driver
-----------------------------------

* Passing a non-:rust:`Copy` handle by value transfers ownership

* Other peripheral fields can still be moved independently

.. code:: rust

  fn initialize_uart(_uart: pac::USART1) {}

  fn initialize() {
      let dp = pac::Peripherals::take().unwrap();
      initialize_uart(dp.USART1);

      // initialize_uart(dp.USART1); // ERROR: already moved
      let _gpioa = dp.GPIOA; // Another field is still usable
  }

-----------------------
Explicit Dependencies
-----------------------

* A driver constructor can require the peripheral it controls

* Its signature documents the resource being transferred

* Tests can substitute a simulated device when the API supports it

.. code:: rust

  // Illustrative driver constructor
  fn new_serial(usart: pac::USART1) -> SerialDriver {
      SerialDriver::from_peripheral(usart)
  }

.. note::

  :rust:`SerialDriver` and this constructor are illustrative

-------------------------------
Limits of Exclusive Ownership
-------------------------------

* A unique handle prevents safe duplication through the API

* Ownership alone does not coordinate other access

  * Interrupts, DMA, another core, or raw register access
  * Different pin handles may share a configuration register

* Shared register updates still need an appropriate access strategy

.. tip::

  Avoid sharing when possible; synchronize shared access when needed

---------------------------------
What Ownership Does Not Express
---------------------------------

* Owning a GPIO pin does not describe its configuration

* Input and output APIs may expose different operations

* The pin handle's type can express these restrictions

.. code:: rust

  Pin<Input>   // Reading is exposed by our example API
  Pin<Output>  // Writing is exposed by our example API

.. note::

  These restrictions are choices made by our teaching API
