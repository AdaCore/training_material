==========
Literals
==========

-----------------
String Literals
-----------------

* A :dfn:`literal` is a *textual* representation of a value in the code

.. code:: Ada

   A_Null_String : constant String := "";
      -- two double quotes with nothing inside
   String_Of_Length_One : constant String := "A";
   Embedded_Single_Quotes : constant String
                          := "Embedded 'single' quotes";
   Embedded_Double_Quotes : constant String
                          := "Embedded ""double"" quotes";

.. container:: speakernote

   Note that the last example literal (that has embedded double quotes) is not an example of concatenation!

--------------------------
Decimal Numeric Literals
--------------------------

* Syntax

   .. code::

      decimal_literal ::=
        numeral [.numeral] E [+numeral|-numeral]
      numeral ::= digit {['_'] digit}

.. tip:: Underscore is **not** significant and used for grouping

* **E** (exponent) must always be integer
* Examples

   .. code:: Ada

      12      0       1E6         123_456
      12.0    0.0     3.14159_26  2.3E-4

------------------------
Based Numeric Literals
------------------------

.. code::

   based_literal ::= base # numeral [.numeral] # exponent
   numeral ::= base_digit { '_' base_digit }

* Base can be 2 .. 16
* Exponent is always a base 10 integer

   ::

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

Which statement(s) is (are) legal?

   A. :answermono:`I : constant := 0_1_2_3_4;`
   B. ``F : constant := 12.;``
   C. ``I : constant := 8#77#E+1.0;``
   D. ``F : constant := 2#1111;``

.. container:: animate

   Explanations

   A. Underscores are not significant - they can be anywhere (except first and last character, or next to another underscore)
   B. Must have digits on both sides of decimal
   C. Exponents must be integers
   D. Missing closing \#

