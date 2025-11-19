=========
Summary
=========

---------
Summary
---------

* Heterogeneous types allowed for components
* Default initial values allowed for components

   - Evaluated when each object elaborated, not the type
   - Not evaluated if explicit initial value specified

* Aggregates express literals for composite types

   - Can mix named and positional forms

* Variant records allow flexible records that maintain strong typing

  * Immutable records always use the same discriminant value
  * Mutable records can change their discriminant

    * But only when entire object is being assigned
