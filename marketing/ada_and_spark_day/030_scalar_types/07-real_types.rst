============
Real Types
============

--------------------------------
Declaring Floating Point Types
--------------------------------

**Syntax**

.. code:: bnf

  floating_point_definition ::= 
     type identifier is
        digits static_expression [real_range_specification];
  real_range_specification ::= 
     range static_simple_expression .. static_simple_expression

- *digits* |rightarrow| **minimum** number of significant digits
- **Decimal** digits, not bits

* Compiler chooses representation

  - From **available** floating point types
  - May be **more** accurate, but not less
  - If none available |rightarrow| declaration is **rejected**
