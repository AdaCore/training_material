================
Memory Budgets
================

---------------------------------------------
Storage Location and Capacity Are Different
---------------------------------------------

**Compare where value lives with how it stores its elements**

.. image:: rust_essentials/520_memory_storage_model.svg
   :width: 100%
   :align: center

* Arrays and :rust:`FixedVec` store elements inline, with capacity fixed by their type

* A :rust:`Vec` stores a fixed-size handle to a separate, growable heap buffer

* Fixed-size arrays do not automatically have static storage

-------------------------------------------
Measure the Value and Its Backing Storage
-------------------------------------------

.. code:: rust

  use core::mem::size_of;

  assert_eq!(size_of::<[u16; 64]>(), 128);

* :rust:`size_of::<T>()` includes inline fields and padding

* For :rust:`Vec<T>` or :rust:`String`, it does not include the heap buffer

  * The value stores a pointer, length, and capacity, not its elements

* A full budget also includes simultaneous buffers and temporary storage

* Elements may own additional heap allocations

.. note::

  Fixed capacity does not bound allocations owned by the elements

---------------------------
The Stack Also Has Limits
---------------------------

* Large local buffers can increase peak stack usage

* Nested calls and recursion can add to that peak

* Moving data to the stack does not make its storage free

* Reusing a caller-owned buffer can avoid creating a separate working buffer for each call

.. warning::

  Avoiding heap allocation does not prove that a program cannot exhaust memory
