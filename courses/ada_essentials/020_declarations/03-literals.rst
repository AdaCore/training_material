==========
Literals
==========

------------------
Numeric Literals
------------------

* Syntax

   .. code::

      numeric_literal ::= numeral [.numeral] [exponent]
      exponent ::= E [ + | - ] numeral
      numeral ::= digit {['_'] digit}

.. tip:: Underscore is **not** significant and helpful for grouping

* **E** (exponent) must always be an integer
* Examples

   .. code:: Ada

      12      0       1E6         123_456
      12.0    0.0     3.14159_26  2.3E-4

------------------------
Based Numeric Literals
------------------------

.. code::

   based_literal ::= base # numeral [.numeral] # [exponent]
   numeral ::= base_digit { '_' base_digit }

* Base can be 2 .. 16
* Exponent is always a base 10 integer

   .. code:: Ada

      16#FFF#           => 4095
      2#1111_1111_1111# => 4095 -- With underline
      16#F.FF#E+2       => 4095.0
      8#10#E+3          => 4096 (8 * 8**3)

--------------------------------------------
Comparison to C's Based Literals
--------------------------------------------

* Design in reaction to C issues
* C has **limited** bases support

   - Bases 8, 10, 16
   - No base 2 in standard

* Zero-prefixed octal :code:`0nnn`

   - **Hard** to read
   - **Error-prone**

------
Quiz
------

Which one of the below is a valid numeric literal?

   A. :answermono:`0_1_2_3_4`
   B. ``12.``
   C. ``8#77#E+1.0``
   D. ``2#1111``

.. container:: animate

   Explanations

   A. Underscores are not significant - they can be anywhere (except first and last character, or next to another underscore)
   B. Must have digits on both sides of decimal
   C. Exponents must be integers
   D. Missing closing \#

