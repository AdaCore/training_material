=========
Summary
=========

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

- **User-defined smart pointers**

  - Implement :rust:`Deref` and :rust:`Drop` for custom behaviors
