=========
Summary
=========

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

* Strings are special-case arrays

   - Any single-dimensioned array of some character type is a :dfn:`string type`
   - Language defines types :ada:`String`, :ada:`Wide_String`, :ada:`Wide_Wide_String`
   - Language-defined support defined in Appendix A - :ada:`Ada.Strings`

* Default initialization for large arrays may be expensive!
* Anonymously-typed array objects used in examples for brevity but that doesn't mean you should in real programs
