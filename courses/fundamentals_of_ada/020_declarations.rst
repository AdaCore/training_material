.. |equivalent| replace:: :math:`\iff`

.. role:: c(code)
    :language: C

.. role:: ada(code)
    :language: Ada

**************
Declarations
**************

==============
Introduction
==============

---------------------
Identifiers
---------------------

.. image:: identifier_flow.png
   :width: 60%

.. container:: columns

 .. container:: column

   * Legal identifiers

      .. code:: Ada

         Phase2
         A
         Space_Person

 .. container:: column

   * Not legal identifiers

      .. code:: Ada

         Phase2__1
         A_
         _space_person

----------------
String Literals
----------------
.. |rightarrow| replace:: :math:`\rightarrow`

.. code::

   string_literal ::= "<string content>"

.. code:: Ada

   A_Null_String : constant string := "";
      -- two double quotes with nothing inside
   String_Of_Length_One : constant string := "A";
   Embedded_Single_Quotes : constant string :=
                            "Embedded 'single' quotes";
   Embedded_Double_Quotes : constant string :=
                            "Embedded ""double"" quotes";

.. container:: speakernote

   Note that the last example literal (that has embedded double quotes) is not an example of concatenation!

====================================
Identifiers, Comments, and Pragmas
====================================

----------
Examples
----------

.. include:: examples/020_declarations/identifiers_comments_and_pragmas.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/020_declarations.html#identifiers-comments-and-pragmas`

-------------
Identifiers
-------------

* Syntax

   .. code::

      identifier ::= letter {[underline] letter_or_digit}

* Character set **Unicode** 4.0

   - 8, 16, 32 bit-wide characters

* Case **not significant**

   - `SpacePerson` |equivalent| `SPACEPERSON`
   - but **different** from `Space_Person`

* Reserved words are **forbidden**

----------------
Reserved Words
----------------

.. code:: Ada

   abort          else              null               select
   abs            elsif             of                 separate
   abstract (95)  end               or                 some (2012)
   accept         entry             others             subtype
   access         exception         out                synchronized (2005)
   aliased (95)   exit              overriding (2005)  tagged (95)
   all            for               package            task
   and            function          pragma             terminate
   array          generic           private            then
   at             goto              procedure          type
   begin          if                protected (95)     until (95)
   body           in                raise              use
   case           interface (2005)  range              when
   constant       is                record             while
   declare        limited           rem                with
   delay          loop              renames            xor
   delta          mod               requeue (95)
   digits         new               return
   do             not               reverse

----------
Comments
----------

* Terminate at end of line (i.e., no comment terminator sequence)

   .. code:: Ada

      -- This is a multi-
      -- line comment
      A : B; -- this is an end-of-line comment

---------
Pragmas
---------

* Compiler directives

   - Compiler action *not part of* Ada grammar
   - Only **suggestions**, may be **ignored**
   - Either standard or implementation-defined

* Unrecognized pragmas

   - **No effect**
   - Cause **warning** (standard mode)

* Malformed pragmas are **illegal**

.. code:: Ada

   pragma Page;
   pragma Optimize ( Off );

------
Quiz
------

Which statement is legal?

   A. ``Function : constant := 1;``
   B. :answermono:`Fun_ction : constant := 1;`
   C. ``Fun_ction : constant := --initial value-- 1;``
   D. ``integer Fun_ction;``

.. container:: animate

   Explanations

   A. :ada:`function` is a reserved word
   B. Correct
   C. Cannot have inline comments
   D. C-style declaration not allowed

==================
Numeric Literals
==================

----------
Examples
----------

.. include:: examples/020_declarations/numeric_literals.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/020_declarations.html#numeric-literals`

--------------------------
Decimal Numeric Literals
--------------------------

* Syntax

   .. code::

      decimal_literal ::=
        numeral [.num] E [+numeral|-numeral]
      numeral ::= digit {[underline] digit}

* Underscore is not significant
* Examples

   .. code:: Ada

      12      0       1E6         123_456
      12.0    0.0     3.14159_26  2.3E-4

------------------------
Based Numeric Literals
------------------------

.. code::

   based_literal ::=
     base # based_number [.based_number] # E [+numeral|-numeral]
   based_number ::= base_digit { '_' base_digit }

* Base can be 2 .. 16
* Exponent is always a decimal number

   .. code:: Ada

      16#FFF#           => 4095
      2#1111_1111_1111# => 4095 -- With underline
      16#F.FF#E+2       => 4095.0
      8#10#E+3          => 4096 (8 * 8**3)

--------------------------------------------
Comparison To C's Based Literals
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

Which statement is legal?

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

=====================
Object Declarations
=====================

----------
Examples
----------

.. include:: examples/020_declarations/object_declarations.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/020_declarations.html#object-declarations`

--------------
Declarations
--------------

* Associate a **name** to an **entity**

    - Objects
    - Types
    - Subprograms
    - et cetera

* Declaration **must precede** use
* **Some** implicit declarations

    - **Standard** types and operations
    - **Implementation**-defined

---------------------
Object Declarations
---------------------

* Variables and constants
* Basic Syntax

   .. code:: Ada

      <name> : subtype_indication [:= <initial value>];

* Examples

   .. code:: Ada

      Z, Phase : Analog;
      Max : constant Integer := 200;
      -- variable with a constraint
      Count : Integer range 0 .. Max := 0;
      -- dynamic initial value via function call
      Root : Tree := F(X);

------------------------------
Multiple Object Declarations
------------------------------

* Allowed for convenience

   .. code:: Ada

      A, B : Integer := Next_Available(X);

* Identical to series of single declarations

   .. code:: Ada

      A : Integer := Next_Available(X);
      B : Integer := Next_Available(X);

* Warning: may get different value

   .. code:: Ada

      T1, T2 : Time := Current_Time;

-------------------------
Predefined Declarations
-------------------------

* **Implicit** declarations
* Language standard
* Annex A for *Core*

   - Package :code:`Standard`
   - Standard types and operators

        + Numerical
        + Characters

   - About **half the RM** in size

* "Specialized Needs Annexes" for *optional*
* Also, implementation specific extensions

------------------------------------
Implicit vs. Explicit Declarations
------------------------------------

* Explicit |rightarrow| in the source

   .. code:: Ada

      type Counter is range 0 .. 1000;

* Implicit |rightarrow| **automatically** by the compiler

   .. code:: Ada

      function "+" ( Left, Right : Counter ) return Counter;
      function "-" ( Left, Right : Counter ) return Counter;
      function "*" ( Left, Right : Counter ) return Counter;
      function "/" ( Left, Right : Counter ) return Counter;
      ...

-------------
Elaboration
-------------

* Effects of the declaration

    - **Initial value** calculations
    - *Execution* at **run-time** (if at all)

* Objects

   - Memory **allocation**
   - Initial value

* Linear elaboration

   - Follows the program text
   - Top to bottom

   .. code:: Ada

      declare
        First_One : Integer := 10;
        Next_One : Integer := First_One;
        Another_One : Integer := Next_One;
      begin
        ...

------
Quiz
------

Which block is illegal?

   A. ``A, B, C : integer;``
   B. ``Integer : Standard.Integer;``
   C. :answermono:`Null : integer := 0;`
   D. | ``A : integer := 123;``
      | ``B : integer := A * 3;``

.. container:: animate

   Explanations

   A. Multiple objects can be created in one statement
   B. :ada:`integer` is *predefined* so it can be overridden
   C. :ada:`null` is *reserved* so it can **not** be overridden
   D. Elaboration happens in order, so :ada:`B` will be 369

=================
Universal Types
=================

-----------------
Universal Types
-----------------

* Implicitly defined
* Entire *classes* of numeric types

   - `universal_integer`
   - `universal_real`
   - `universal_fixed`

* Match any integer / real type respectively

   - **Implicit** conversion, as needed

  .. code:: Ada

     X : Integer64 := 2;
     Y : Integer8 := 2;

----------------------------------------
Numeric Literals Are Universally Typed
----------------------------------------

* No need to type them

   - e.g :code:`0UL` as in C

* Compiler handles typing

   - No bugs with precision

  .. code:: Ada

     X : Unsigned_Long := 0;
     Y : Unsigned_Short := 0;

----------------------------------------
Literals Must Match "Class" of Context
----------------------------------------

* `universal_integer` literals |rightarrow| **integer**
* `universal_real` literals |rightarrow| **fixed** or **floating** point
* Legal

  .. code:: Ada

     X : Integer := 2;
     Y : Float := 2.0;

* Not legal

  .. code:: Ada

     X : Integer := 2.0;
     Y : Float := 2;

===============
Named Numbers
===============

----------
Examples
----------

.. include:: examples/020_declarations/named_numbers.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/020_declarations.html#named-numbers`

---------------
Named Numbers
---------------

* Associate a **name** with an **expression**

   - Used as **constant**
   - `universal_integer`, or `universal_real`
   - compatible with integer / real respectively
   - Expression must be **static**

* Syntax

   .. code:: Ada

     <name> : constant := <static_expression>;

* Example

   .. code:: Ada

      Pi : constant := 3.141592654;
      One_Third : constant := 1.0 / 3.0;

--------------------------------------
A Sample Collection of Named Numbers
--------------------------------------

.. code:: Ada

   package Physical_Constants is
     Polar_Radius : constant := 20_856_010.51;
     Equatorial_Radius : constant := 20_926_469.20;
     Earth_Diameter : constant :=
       2.0 * ((Polar_Radius + Equatorial_Radius)/2.0);
     Gravity : constant := 32.1740_4855_6430_4;
     Sea_Level_Air_Density : constant :=
       0.002378;
     Altitude_Of_Tropopause : constant := 36089.0;
     Tropopause_Temperature : constant := -56.5;
   end Physical_Constants;

----------------------
Named Number Benefit
----------------------

* Evaluation at **compile time**

    - As if **used directly** in the code
    - **Perfect** accuracy

  .. code:: Ada

    Named_Number   : constant :=       1.0 / 3.0;
    Typed_Constant : constant float := 1.0 / 3.0;

.. container:: columns

 .. container:: column

    * :code:`Named_Number` value

       - as a 32 bits Float |rightarrow| 3.33333E-01
       - as a 64 bits Float |rightarrow| 3.33333333333333E-01
       - as a 128 bits Float |rightarrow| 3.33333333333333333E-01

 .. container:: column

    * :code:`Typed_Constant` value

       - as a 32 bits Float |rightarrow| 3.33333E-01
       - as a 64 bits Float |rightarrow| 3.333333_43267441E-01
       - as a 128 bits Float |rightarrow| 3.333333_43267440796E-01

..
    TODO Try using a table

======================
Scope and Visibility
======================

----------
Examples
----------

.. include:: examples/020_declarations/scope_and_visibility.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/020_declarations.html#scope-and-visibility`

----------------------
Scope and Visibility
----------------------

* **Scope** of a name

   - Where the name is **potentially** available
   - Determines **lifetime**
   - Scopes can be **nested**

* **Visibility** of a name

   - Where the name is **actually** available
   - Defined by **visibility rules**
   - **Hidden** |rightarrow| *in scope* but **not visible**

------------------------------
Introducing Block Statements
------------------------------

* **Sequence** of statements

   - Optional *declarative part*
   - Can be **nested**
   - Declarations **can hide** outer variables

.. container:: columns

 .. container:: column

    * Syntax

       .. code:: Ada

          [<block-name> :] declare
             <declarative part>
          begin
             <statements>
          end [block-name];

 .. container:: column

    * Example

       .. code:: Ada

          Swap: declare
            Temp : Integer;
          begin
            Temp := U;
            U := V;
            V := Temp;
          end Swap;

----------------------
Scope and "Lifetime"
----------------------

* Object in scope |rightarrow| exists
* No *scoping* keywords

    - C's :c:`static`, :c:`auto` etc...

.. image:: block_scope_example.jpeg
    :height: 50%

-------------
Name Hiding
-------------

* Caused by **homographs**

    - **Identical** name
    - **Different** entity

   .. code:: Ada

      declare
        M : Integer;
      begin
        ... -- M here is an INTEGER
        declare
          M : Float;
        begin
          ... -- M here is a FLOAT
        end;
        ... -- M here is an INTEGER
      end;

-------------------
Overcoming Hiding
-------------------

* Add a **prefix**

   - Needs named scope

* Homographs are a *code smell*

    - May need **refactoring**...

.. code:: Ada

   Outer : declare
     M : Integer;
   begin
     ...
     declare
       M : Float;
     begin
       Outer.M := Integer(M); -- Prefixed
     end;
     ...
   end Outer;

------
Quiz
------

.. container:: columns

 .. container:: column

  .. container:: latex_environment footnotesize

   What output does the following code produce? (Assume :code:`Print` prints the current value of its argument)

   .. code:: Ada

      declare
         M : Integer := 1;
      begin
         M := M + 1;
         declare
            M : Integer := 2;
         begin
            M := M + 2;
            Print ( M );
         end;
         Print ( M );
      end;

 .. container:: column

   A. 2, 2
   B. 2, 4
   C. 4, 4
   D. :answer:`4, 2`

   .. container:: animate

      Explanation

      * Inner :ada:`M` gets printed first. It is initialized to 2 and incremented by 2
      * Outer :ada:`M` gets printed second. It is initialized to 1 and incremented by 1

================
Aspect Clauses
================

----------
Examples
----------

.. include:: examples/020_declarations/aspect_clauses.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/020_declarations.html#aspect-clauses`

----------------
Aspect Clauses
----------------

.. admonition:: Language Variant

   Ada 2012

* Define **additional** properties of an entity

    - Representation (eg. :code:`Packed`)
    - Operations (eg. :code:`Inline`)
    - Can be **standard** or **implementation**-defined

* Usage close to pragmas

    - More **explicit**, **typed**
    - **Cannot** be ignored
    - **Recommended** over pragmas

* Syntax

    - *Note:* always part of a **declaration**

   .. code:: Ada

      with aspect_mark [ => expression]
           {, aspect_mark [ => expression] }

--------------------------------
Aspect Clause Example: Objects
--------------------------------

.. admonition:: Language Variant

   Ada 2012

* Updated **object syntax**

   .. code:: Ada

      <name> : <subtype_indication> [:= <initial value>]
                     with aspect_mark [ => expression]
                     {, aspect_mark [ => expression] };

* Usage

   .. code:: Ada

      CR1 : Control_Register with
         Size    => 8,
         Address => To_Address (16#DEAD_BEEF#);

      -- Prior to Ada 2012
      -- using *representation clauses*
      CR2 : Control_Register;
      for CR2'Size use 8;
      for CR2'Address use To_Address (16#DEAD_BEEF#);

------------------------
Boolean Aspect Clauses
------------------------

.. admonition:: Language Variant

   Ada 2012

* **Boolean** aspects only
* Longhand

  .. code:: Ada

     procedure Foo with Inline => True;

* Aspect name only |rightarrow| **True**

  .. code:: Ada

     procedure Foo with Inline; -- Inline is True

* No aspect |rightarrow| **False**

  .. code:: Ada

     procedure Foo; -- Inline is False

  - Original form!

=========
Summary
=========

---------
Summary
---------

* Declarations of a **single** type, permanently

   - OOP adds flexibility

* Named-numbers

    - **Infinite** precision, **implicit** conversion

* **Elaboration** concept

    - Value and memory initialization at **run-time**

* Simple **scope** and **visibility** rules

    - **Prefixing** solves **hiding** problems

* Pragmas, Aspects
* Detailed syntax definition in Annex P (using BNF)

