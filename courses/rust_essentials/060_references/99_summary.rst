=========
Summary
=========

-----------------
What We Covered
-----------------

- **Shared References** (:rust:`&T`)

  - Allow read-only access to value
  - Implemented as safe pointers
  
- **Mutable References** (:rust:`&mut T`)

  - Allow read-write access
  - Only one reference allowed at a time

- **Safety**
  - References are never null and are always valid

- **Slices** (:rust:`&[T]`)

  - Efficient views into arrays or other collections
  - :rust:`&str` is a slice      
