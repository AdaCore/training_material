=========
Summary
=========

----------------------
"Box<T>" vs. "Rc<T>"
----------------------

.. list-table::
   :header-rows: 1
   :stub-columns: 1

   * - **Property**
     - :rust:`Box<T>`
     - :rust:`Rc<T>`

   * - *Ownership*
     - Single
     - Multiple

   * - *Allocation*
     - Heap
     - Heap

   * - *If cloned*
     - Clones value
     - Shares allocation

   * - *Use case*
     - Unique / recursive
     - Shared ownership

-----------------
What We Covered
-----------------

- **Unique Ownership** (:rust:`Box<T>`)

  - Provides unique ownership of heap-allocated data
  - Enables recursive data structures

- **Dereferencing**

  - :rust:`Deref` supports dereferencing and coercion
  - :rust:`DerefMut` supports mutable dereferencing

- **Shared Ownership** (:rust:`Rc<T>`)

  - Allows multiple owners for the same heap allocation
  - :rust:`Rc::clone` adds an owner without cloning the data
