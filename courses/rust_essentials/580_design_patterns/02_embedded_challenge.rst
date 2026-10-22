========================
The Embedded Challenge
========================

-----------------------
Shared Hardware State
-----------------------

* Memory-mapped registers control hardware through fixed addresses

* Several parts of a program can access the same register

* Uncoordinated access can interfere with other code, including interrupts

------------------------
Direct Register Access
------------------------

**A read-modify-write operation can interfere with another access**

.. code:: c

  #include <stdint.h>

  // Illustrative STM32F401 access, not complete GPIO setup
  #define GPIOA_MODER (*(volatile uint32_t *)0x40020000u)

  void change_mode_bit(void) {
      GPIOA_MODER |= (1u << 10);
  }

.. note::

  :C:`volatile` does not provide synchronization

-----------------------
Why Ownership Matters
-----------------------

* Direct register access does not establish responsibility for a peripheral

* Dereferencing a raw pointer requires an unsafe context

* A peripheral API can provide a handle that is moved or borrowed

* Drivers receive hardware dependencies explicitly

  * They do not need to access hidden global state

.. note::

  :rust:`unsafe` does not synchronize register access

---------------------------
PACs and HALs: A Reminder
---------------------------

* A **peripheral access crate (PAC)** exposes device registers and handles

  * Includes register fields for low-level access

* A **hardware abstraction layer (HAL)** provides higher-level operations

  * May enforce configuration rules

* Ownership and typestate can help a HAL provide a safer API
