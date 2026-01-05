=========
Summary
=========

-----------------
What We Covered
-----------------

- **Arrays**
  - Group values of same type, fixed length
  - Length is part of the type
  - Initialization with literals (e.g., :rust:`[2, 3, 5]`)
    - Or the :rust:`[value; N]` syntax
  - Looping over arrays with :rust:`for`
  
  **Tuples**
  - Group values of different types, fixed length
  - Fields are accessed via dot notation followed by their index
    - Starting from :rust:`0`

-----------------------------
What We Covered (Continued)
-----------------------------

- **Patterns and Destructuring**
  - Used for clarity, simplicity and skipping unneeded values
  - Irrefutable patterns are patterns guaranteed to match the structure 
  - Skip an element using (:rust:`_`) and multiple elements using (:rust:`..`)
  - Destructuring can be used as an assignment operation
  - Use a pattern within a pattern to destructure an array of arrays
  