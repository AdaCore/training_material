=========
Summary
=========

--------------------------------
Comparing "Rc<T>" and "Box<T>"
--------------------------------

.. list-table::
   :header-rows: 1
   
   * - **Properties**
     - :rust:`Box<T>`
	 - :rust:`Rc<T>`

   * - *Ownership*
     - Single
	 - Multiple

   * - *Access*
     - Mutable
	 - Immutable

   * - *Memory location*
     - Heap
	 - Heap
   
   * - *Cloning*
     - Deep Copy
	 - Shallow Copy

   * - *Main Use Case*
     - Big data/recursive types
	 - Complex architecture
	
-----------------
What We Covered
-----------------

- :rust:`Box<T>`

  - Allocates data on the heap
  
  - Enables recursive data structures

- :rust:`Deref`

  - Treats smart pointers like references
  
  - Uses coercion to access inner values 
   
    - With no runtime cost

- :rust:`Rc<T>`

  - Allows multiple owners for the same data
  
  - Avoids expensive cloning by reusing the same heap allocation


