=========
Summary
=========

------------------------------
Final Notes on Type `String`
------------------------------

* Any single-dimensioned array of some character type is a :dfn:`string type`

   - Language defines types `String`, `Wide_String`, etc.

* Just another array type: no null termination
* Language-defined support defined in Appendix A

   - `Ada.Strings.*`
   - Fixed-length, bounded-length, and unbounded-length
   - Searches for pattern strings and for characters in program-specified sets
   - Transformation (replacing, inserting, overwriting, and deleting of substrings)
   - Translation (via a character-to-character mapping)

---------
Summary
---------

* Any dimensionality directly supported
* Component types can be any (constrained) type
* Index types can be any discrete type

   - Integer types
   - Enumeration types

* Constrained array types specify bounds for all objects
* Unconstrained array types leave bounds to the objects

   - Thus differently-sized objects of the same type

* Default initialization for large arrays may be expensive!
* Anonymously-typed array objects used in examples for brevity but that doesn't mean you should in real programs
