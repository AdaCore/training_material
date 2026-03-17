=========
Summary
=========

-----------------
What We Covered
-----------------

- :rust:`Box<T>`

  - Allocates data on the heap for flexible storage 
  
  - Enables recursive data structures
   
    - Using fixed-size pointers 

- :rust:`Deref`

  - Treats smart pointers like references
   
    - Creating cleaner code
  
  - Uses coercion to access inner values 
   
    - With no runtime cost

- **User-defined smart pointers**

  - implement :rust:`Deref` and :rust:`Drop` for custom behaviors

- :rust:`Rc<T>`

  - Allows multiple parts of a program to own the same data
  
  - Avoids expensive cloning by reusing the same heap allocation
