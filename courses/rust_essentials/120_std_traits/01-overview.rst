==========
Overview
==========

--------------------------
Review: What is a Trait?
--------------------------

* Contract for Behavior

  * Defines a set of methods type must implement

* Abstraction Tool

  * Allows programmer to write code that functions on many different types

    * As long as they provide the required behavior

* Similar to *interfaces* in Java/C# or *abstract base classes* in Python/C++

  * More flexible

----------------------------------------
New: What is a Standard Library Trait?
----------------------------------------

* Predefined traits in Rust’s Standard Library

  * Describe common behaviors
  * Integrate user types with both language features and library APIs

* What are they used for?

  * Define standardized capabilities

    * E.g., comparing, converting, formatting

  * Allows user types to work with built-in syntax and library functions


------------------------------
Preview: What Will We Cover?
------------------------------

* Comparisons

  * :rust:`PartialEq`, :rust:`Eq`, :rust:`PartialOrd`, :rust:`Ord` 

    * Enable **==**, **<**, sorting

* Conversions

  * :rust:`From`, :rust:`Into`, :rust:`TryFrom`

    * Convert between types

* Operators & Casting

  * Traits like :rust:`Add`, :rust:`Sub`, numeric casts

* I/O Traits

  * :rust:`Read`, :rust:`Write`

    * For streams and files

* Defaults

  * :rust:`Default`

    * Create a "zero value"
