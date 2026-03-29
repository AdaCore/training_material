=========
Summary
=========

-----------------
What We Covered
-----------------

- **Borrowing Rules**
  - Rust guarantees safety by enforcing strict access
    - Either multiple readers (:rust:`&T`)
    - Or exactly one writer (:rust:`&mut T`)
  
- **Local, Function, and Method Borrows**
  - The exact same borrowing rules apply whether you are
    - Assigning local variables
    - Passing arguments to functions
    - Or calling methods via :rust:`&self` or :rust:`&mut self`

- **Compile-Time Safety**
  - Overlapping mutable or immutable references are strictly checked

- **Interior Mutability**
  - :rust:`Cell<T>` and :rust:`RefCell<T>` allow safe data mutation through shared immutable references
    - When strict compile-time rules are too inflexible
