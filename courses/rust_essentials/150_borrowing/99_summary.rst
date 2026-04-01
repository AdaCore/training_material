=========
Summary
=========

-----------------
What We Covered
-----------------

- **Borrowing Rules**
  - Guarantee safety by enforcing strict access
  
- **Local, Function, and Method Borrows**
  - Exact same borrowing rules apply whether you are
    - Assigning local variables
    - Passing arguments to functions
    - Calling methods via :rust:`&self` or :rust:`&mut self`

- **Compile-Time Safety**
  - Overlapping mutable or immutable references are strictly checked

- **Interior Mutability**
  - Allows safe data modification through shared immutable references
