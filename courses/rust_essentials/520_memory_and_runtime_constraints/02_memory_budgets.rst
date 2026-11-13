================
Memory Budgets
================

-------------------------
Inline and Heap Storage
-------------------------

**Two local values can store their elements differently**

.. code:: rust

  let array = [10_u16, 42, 7, 18];
  let vector = vec![10_u16, 42, 7, 18];

* :rust:`array` stores its four elements directly in the value

  * Its length is fixed by its type, :rust:`[u16; 4]`

* :rust:`vector` stores a pointer, length, and capacity

  * Its elements are in a separate heap allocation that may grow

* Fixed-size arrays can also have static storage (covered later)

---------------------------------
Value Size vs. Memory Footprint
---------------------------------

**Vector size does not include its heap buffer**

.. code:: rust

  use core::mem::size_of;

  let array_bytes = size_of::<[u16; 4]>();
  let vector_bytes = size_of::<Vec<u16>>();

  println!("Array: {array_bytes} bytes");
  println!("Vec value: {vector_bytes} bytes");

* Example output on a typical 64-bit target

.. code:: output

  Array: 8 bytes
  Vec value: 24 bytes

* :rust:`size_of::<T>()` counts the value's storage, including padding

--------------------
Heap Buffer Memory
--------------------

**Heap buffer memory depends on capacity and element size**

.. code:: rust

  use core::mem::size_of;

  let vector = vec![10_u16, 42, 7, 18];
  let buffer_bytes = vector.capacity() * size_of::<u16>();

  println!("Vec buffer: {buffer_bytes} bytes");

* Example output

.. code:: output

  Vec buffer: 8 bytes

* :rust:`capacity()` counts elements that fit before growth

.. note::

  Other live buffers and allocator overhead add to memory use

---------------------
Stack Memory Limits
---------------------

* Heap-free code can still exceed the available stack budget

* Stack overflow can cause program failure

* Peak stack usage can increase with:

  * Large local arrays and working buffers

  * Nested function calls and recursion

* Caller-provided buffers can avoid an additional local working buffer

.. tip::

  Estimate worst-case stack usage, not just individual value sizes
