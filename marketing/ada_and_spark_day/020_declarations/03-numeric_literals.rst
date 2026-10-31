==================
Numeric Literals
==================

------------------
Decimal Literals
------------------

**Syntax**

  .. code:: bnf

    decimal_literal ::= numeral [.numeral] [exponent]
    numeral ::= digit {[_] digit}
    exponent ::= E [+] numeral | E - numeral

  *Underscore is* **not** *significant but helpful for grouping*

  **E** *(exponent) must always be an integer*

* Examples

  .. code:: Ada

     12      0       1E6         123_456
     12.0    0.0     3.14159_26  2.3E-4

----------------
Based Literals
----------------

**Syntax**

  .. code:: bnf

    based_literal ::= 
       base # based_numeral [.based_numeral] # [exponent]
    base ::= numeral
    based_numeral ::= extended_digit {[_] extended_digit}
    extended_digit ::= digit | A | B | C | D | E | F

  * Base can be 2 .. 16
  * Exponent is always a base 10 integer

* Examples

  .. code::

     16#FFF#           => 4095
     2#1111_1111_1111# => 4095 -- With underscore
     16#F.FF#E+2       => 4095.0
     8#10#E+3          => 4096 (8 * 8**3)
