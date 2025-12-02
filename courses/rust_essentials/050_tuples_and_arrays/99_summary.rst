=========
Summary
=========

-----------------
What We Covered
-----------------

- **Arrays**
  - Group values of same type, fixed size
  - Length is part of the type
  - Initialization with literals (e.g., :rust:`[2, 3, 5]`) or the :rust:`[value; N]` syntax
  - Looping over arrays with :rust:`for` using the :rust:`IntoIterator` trait
  
  **Tuples**
  - Group values of same type, fixed size
  - Fields are accessed by a dot ( :rust:`.`) followed by their index, starting from :rust:`0`
  - The empty tuple (:rust:`()`) is called the unit type.

- **Patterns & Destructuring**
  - Used for clarity, simplicity and skipping uneeded values
  - Irrefutable patterns are patterns guaranteed to match the structure 
  - Skip specific element using (:rust:`_`)
  - Skip multiple elements using (:rust:`..`)
  - Destructuring can be used as an assignment operation
  - Use a pattern within a pattern to destructure an array of arrays
