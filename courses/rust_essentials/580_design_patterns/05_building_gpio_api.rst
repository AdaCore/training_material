=====================
Building a GPIO API
=====================

------------------
Our Teaching API
------------------

**The following snippets illustrate a HAL-like design**

#. One peripheral handle produces distinct pin handles

#. Each pin starts in a HAL-defined :rust:`Unconfigured` state

#. Transitions consume the previous handle

#. Only :rust:`Pin<Input>` exposes :rust:`is_high()`

#. Only :rust:`Pin<Output>` exposes :rust:`set_high()`

.. note::

  Device-specific register operations are omitted

--------------------------------
What Does "Unconfigured" Mean?
--------------------------------

* :rust:`Unconfigured` describes the driver's knowledge of the pin

* The driver has not established an input or output configuration

* The chip's physical reset mode may already have a configuration

.. note::

  A state type does not inspect or change hardware by itself

-----------------------------------
Step 1: Create Unique Pin Handles
-----------------------------------

* A public :rust:`Pin::new(number)` could duplicate a physical pin handle

* Keep :rust:`Pin`'s fields private

* Produce each pin handle once from the GPIO peripheral

.. code:: rust

  // Inside the illustrative gpio module
  pub struct Pins {
      pub pa5: Pin<Unconfigured>,
      pub pa6: Pin<Unconfigured>,
  }

-------------------------------
Splitting the GPIO Peripheral
-------------------------------

**Consume the peripheral handle to create distinct pin handles**

.. code:: rust

  // Inside the same gpio module
  pub fn split_gpioa(_gpioa: pac::GPIOA) -> Pins {
      Pins {
          pa5: Pin { number: 5, _mode: PhantomData },
          pa6: Pin { number: 6, _mode: PhantomData },
      }
  }

* :rust:`Pin` does not implement :rust:`Copy` or :rust:`Clone`

* Callers can move each public pin field independently

----------------------------
Step 2: Consume the Handle
----------------------------

* Passing :rust:`self` by value moves the original handle

* The returned handle has a type representing the new configuration

.. code:: rust

  impl Pin<Unconfigured> {
      pub fn into_output(self) -> Pin<Output> {
          // Real HAL: configure the pin as an output here
          Pin { number: self.number, _mode: PhantomData }
      }
  }

.. note::

  A real driver must establish the configuration before returning

---------------------
An Input Transition
---------------------

* The same approach produces a handle for input operations

.. code:: rust

  impl Pin<Unconfigured> {
      pub fn into_input(self) -> Pin<Input> {
          // Real HAL: configure the pin as an input here
          Pin { number: self.number, _mode: PhantomData }
      }
  }

* Both transitions consume a :rust:`Pin<Unconfigured>`

* The caller cannot use the old handle after either transition

----------------------------------
Step 3: Expose Output Operations
----------------------------------

* An :rust:`impl` block for :rust:`Pin<Output>` restricts method availability

.. code:: rust

  impl Pin<Output> {
      pub fn set_high(&mut self) {
          // Real HAL: write to the output register here
          todo!("device-specific output write")
      }
  }

* Our teaching API exposes writing only for :rust:`Output`

.. note::

  :rust:`todo!()` marks an omitted operation and panics if reached

-------------------------
Expose Input Operations
-------------------------

* A separate :rust:`impl` block provides the input operation

.. code:: rust

  impl Pin<Input> {
      pub fn is_high(&self) -> bool {
          // Real HAL: read the input register here
          todo!("device-specific input read")
      }
  }

* Our teaching API exposes reading only for :rust:`Input`

* The hardware read is omitted from this illustrative method

---------------------
Step 4: Use the API
---------------------

.. code:: rust

  // Illustrative use of the preceding API
  let dp = pac::Peripherals::take().unwrap();
  let pins = gpio::split_gpioa(dp.GPIOA);

  let mut led = pins.pa5.into_output();
  let button = pins.pa6.into_input();

  led.set_high();
  let _pressed = button.is_high();

* One handle for each physical pin

* The transition determines which methods can be called

--------------------------------
Invalid Operation: Wrong State
--------------------------------

.. code:: rust

  let button = pins.pa6.into_input();

  button.set_high(); // ERROR: no set_high() on Pin<Input>

* The method is implemented for :rust:`Pin<Output>`

  * It is unavailable for :rust:`Pin<Input>`

* The compiler rejects the call without running the program

---------------------------------
Invalid Operation: Moved Handle
---------------------------------

.. code:: rust

  let unconfigured = pins.pa5;
  let mut led = unconfigured.into_output();

  led.set_high();             // Available on Pin<Output>
  unconfigured.into_input(); // ERROR: use of moved value

* :rust:`into_output(self)` consumes the original handle

* Rust prevents using that handle through its old binding

--------------------------------------
A Safe API Requires a Correct Driver
--------------------------------------

* Typestate checks types, without inspecting hardware registers

* The driver must establish the advertised configuration

* Register operations must respect device requirements

  * Shared access must be coordinated
  * Unsafe operations retain their safety obligations

.. warning::

  Type checking alone cannot verify the hardware configuration

----------------------------------
Type Safety vs. Hardware Reality
----------------------------------

* A correctly implemented safe HAL can prevent misuse through its public API

* It cannot prevent external changes or arbitrary unsafe access

* A mode type alone cannot make every hardware operation safe

.. tip::

  Ownership controls access; typestate controls available operations
