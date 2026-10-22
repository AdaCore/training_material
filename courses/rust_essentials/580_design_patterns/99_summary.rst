=========
Summary
=========

--------------------------------
Recap: Ownership and Typestate
--------------------------------

* **Acquisition**

  * A PAC can provide one-time safe access to peripheral handles

* **Ownership**

  * Moving a handle transfers responsibility for the resource

* **Typestate**

  * The handle's type controls which operations the API exposes

* **Transitions**

  * Consuming methods return a handle with a new state type

* **Correctness**

  * The driver must keep the represented state consistent with hardware

-----------------
What We Covered
-----------------

* **Peripheral Ownership**

  - Handles support one-time acquisition and explicit dependencies
  - Shared registers and concurrency still need coordination

* **The Typestate Pattern**

  - ZST markers and :rust:`PhantomData` associate a state with a type
  - State-specific :rust:`impl` blocks restrict method availability

* **Building a GPIO API**

  - Unique handles and consuming transitions prevent invalid handle reuse

* **Typestate in Practice**

  - :rust:`embedded-hal` traits support portable drivers
  - Type erasure can preserve mode restrictions
