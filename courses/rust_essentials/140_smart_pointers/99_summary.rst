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

- :rust:`Box<T>`

  - Provides unique ownership of heap-allocated data

  - Enables recursive data structures

- :rust:`Deref`

  - Treats smart pointers like references

  - Uses coercion to access inner values

    - With no runtime cost

- :rust:`Rc<T>`

  - Allows multiple owners for the same data

  - Shares one heap allocation without cloning the data
