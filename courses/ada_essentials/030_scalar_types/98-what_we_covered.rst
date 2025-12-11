=================
What We Covered
=================

-------------------------------
What We Covered About Numbers
-------------------------------

* Integer-based types

  * Signed integers

    * Numeric types are defined by their range
    * Compiler chooses the number of bits (but we can force the issue)

  * Modular types

    * Unsigned types can be treated as numbers that wrap on underflow/overflow
    * But we can treat them as bits using bitwise or shift operations

* Floating-point types

  * By using precision to define the type, compiler picks size needed
  * We can still add ranges to constrain the range of values

----------------------
What Else We Covered
----------------------

* Enumeration types

  * Used to represent states by a name rather than a value
  * Names in different enumerations are not a problem
  * Characters are just a special case of enumerations

* Attributes

  * Some query the type for range information (e.g. :ada:`'First')
  * Some perform common operations on the type (e.g. :ada:`'Image`)

* Subtypes create constrained version of type but adds constraints

* Derived types create similar copies of type that aren't the same but are convertible
