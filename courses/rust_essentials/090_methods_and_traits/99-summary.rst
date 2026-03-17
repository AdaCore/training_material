=========
Summary
=========

-----------------
What We Covered
-----------------

* **Methods**

  * Functions tied directly to a specific type via :rust:`impl` blocks
  * Use :rust:`self` receivers to define how data is accessed

    * Shared, mutable, or owned

  * Associated functions (no :rust:`self`) act as constructors or helpers

* **Traits**

  * Act as *contracts* or interfaces for shared behavior across different types
  * Can provide default implementations to reduce repetitive code

* **Deriving**

  * Uses :rust:`#[derive]` to auto-generate implementations for common traits
  * Saves boilerplate for standard tasks

    * Debugging, cloning, and comparisons

  * Functions based on the internal structure of the type

