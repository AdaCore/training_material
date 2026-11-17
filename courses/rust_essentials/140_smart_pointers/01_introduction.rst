==============
Introduction
==============

----------------
Topics Covered
----------------

- **Unique Ownership** (:rust:`Box<T>`)

  - Heap allocation and recursive types
  - Fixed-length boxed slices

- **Dereferencing**

  - :rust:`Deref`, :rust:`DerefMut`, and coercion

- **Shared Ownership** (:rust:`Rc<T>`)

  - Reference-counted ownership

---------------------
Why Smart Pointers?
---------------------

- Store data on the heap

- Allow recursive types
  - Give recursive fields a known size

- Allow multiple owners
  - Share ownership of data for complex architectures
