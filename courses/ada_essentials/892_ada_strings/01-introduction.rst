==============
Introduction
==============

-------------------------------
Predefined Type :ada:`String`
-------------------------------

* :ada:`String` type allows varying lengths, but :ada:`String` objects are fixed lengths

  * It's just an unconstrained array of characters

* Language does not have any built-in string manipulation subprograms

* What if we want to change the length of the object?

--------------------------
:ada:`Ada.Strings.Fixed`
--------------------------

* Based on fixed-length string

* Strings are unconstrained arrays, so objects cannot change length

* Operations that return string of unknown (or different) length can only be used for initialization

----------------------------
:ada:`Ada.Strings.Bounded`
----------------------------

* Contains generic package

  * Must create instance passing in maximum string length

* String length is maintained internally

  * Operations can modify objects in-place
  * Subject to limit of maximum length

* Contains query to get maximum length

  * Allows client to pre-determine if length will be exceeded

------------------------------
:ada:`Ada.Strings.Unbounded`
------------------------------

* Not a generic package

  * No maximum length (except runtime limits!)

* String length is maintained internally

  * Operations can modify objects in-place
  * Subject to limit of maximum length

* Requires dynamic memory allocation

