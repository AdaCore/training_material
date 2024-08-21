**************
Declarations
**************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

------------
Declarations
------------

* :dfn:`Declaration` associates a :dfn:`name` to an :dfn:`entity`

    - Objects
    - Types
    - Subprograms
    - et cetera

* In a :dfn:`declarative part`
* Example: :ada:`N : Type := Value;`

    - ``N`` is usually an :dfn:`identifier`

* Declaration **must precede** use
* **Some** implicit declarations

    - **Standard** types and operations
    - **Implementation**-defined

==========================
Identifiers and Comments
==========================

-----------
Identifiers
-----------

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

* Character set **Unicode** 4.0
* Case **not significant**

   - `SpacePerson` |equivalent| `SPACEPERSON`
   - but **different** from `Space_Person`

* Reserved words are **forbidden**

----------------
Reserved Words
----------------

.. code:: Ada

   abort          else              null               reverse
   abs            elsif             of                 select
   abstract (95)  end               or                 separate
   accept         entry             others             some (2012)
   access         exception         out                subtype
   aliased (95)   exit              overriding (2005)  synchronized (2005)
   all            for               package            tagged (95)
   and            function          parallel (2022)    task
   array          generic           pragma             terminate
   at             goto              private            then
   begin          if                procedure          type
   body           in                protected (95)     until (95)
   case           interface (2005)  raise              use
   constant       is                range              when
   declare        limited           record             while
   delay          loop              rem                with
   delta          mod               renames            xor
   digits         new               requeue (95)
   do             not               return

----------
Comments
----------

* Terminate at end of line (i.e., no comment terminator sequence)

   .. code:: Ada

      -- This is a multi-
      -- line comment
      A : B; -- this is an end-of-line comment

----------------------------------------------
Declaring Constants / Variables (simplified)
----------------------------------------------

* An :dfn:`expression` is a piece of Ada code that returns a **value**.

.. code:: Ada

   <identifier> : constant := <expression>;
   <identifier> : <type> := <expression>;
   <identifier> : constant <type> := <expression>;

------
Quiz
------

Which statement is legal?

   A. ``Function : constant := 1;``
   B. :answermono:`Fun_ction : constant := 1;`
   C. ``Fun_ction : constant := --initial value-- 1;``
   D. ``Integer Fun_ction;``

.. container:: animate

   Explanations

   A. :ada:`function` is a reserved word
   B. Correct
   C. Cannot have inline comments
   D. C-style declaration not allowed

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
   Embedded_Single_Quotes : constant String :=
                            "Embedded 'single' quotes";
   Embedded_Double_Quotes : constant String :=
                            "Embedded ""double"" quotes";

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

* Underscore is not significant
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

---------------------
Object Declarations
---------------------

* An :ada:`object` is either :dfn:`variable` or :dfn:`constant`
* Basic Syntax

   .. code:: Ada

      <name> : <subtype> [:= <initial value>];
      <name> : constant <subtype> := <initial value>;

* Constant should have a value

   - Except for privacy (seen later)

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

      A, B : Integer := Next_Available (X);

* Identical to series of single declarations

   .. code:: Ada

      A : Integer := Next_Available (X);
      B : Integer := Next_Available (X);

.. warning:: May get different value!

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
Implicit Vs Explicit Declarations
------------------------------------

* :dfn:`Explicit` |rightarrow| in the source

   .. code:: Ada

      type Counter is range 0 .. 1000;

* :dfn:`Implicit` |rightarrow| **automatically** by the compiler

   .. code:: Ada

      function "+" (Left, Right : Counter) return Counter;
      function "-" (Left, Right : Counter) return Counter;
      function "*" (Left, Right : Counter) return Counter;
      function "/" (Left, Right : Counter) return Counter;
      ...

   * Compiler creates appropriate operators based on the underlying type

      * Numeric types get standard math operators
      * Array types get concatenation operator
      * Most types get assignment operator

-----------
Elaboration
-----------

* :dfn:`Elaboration` has several facets:

  * **Initial value** calculation

    - Evaluation of the expression
    - Done at **run-time** (unless static)

  * Object creation

    - Memory **allocation**
    - Initial value assignment (and type checks)

* Runs in linear order

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

Which block is **not** legal?

   A. ``A, B, C : Integer;``
   B. ``Integer : Standard.Integer;``
   C. :answermono:`Null : Integer := 0;`
   D. | ``A : Integer := 123;``
      | ``B : Integer := A * 3;``

.. container:: animate

   Explanations

   A. Multiple objects can be created in one statement
   B. :ada:`Integer` is *predefined* so it can be overridden
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

   - :ada:`universal_integer`
   - :ada:`universal_real`
   - :ada:`universal_fixed` (not seen here)

* Match any integer / real type respectively

   - **Implicit** conversion, as needed

  .. code:: Ada

     X : Integer64 := 2;
     Y : Integer8 := 2;
     F : Float := 2.0;
     D : Long_Float := 2.0;

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

* `universal_integer` literals |rightarrow| **Integer**
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
    Typed_Constant : constant Float := 1.0 / 3.0;

.. container:: latex_environment footnotesize

  .. list-table::
    :header-rows: 1

    * - Object
      - Named_Number
      - Typed_Constant

    * - :ada:`F32 : Float_32;`
      - 3.33333E-01
      - 3.33333E-01

    * - :ada:`F64 : Float_64;`
      - 3.33333333333333E-01
      - 3.333333_43267441E-01

    * - :ada:`F128 : Float_128;`
      - 3.33333333333333333E-01
      - 3.333333_43267440796E-01

======================
Scope and Visibility
======================

----------------------
Scope and Visibility
----------------------

* :dfn:`Scope` of a name

   - Where the name is **potentially** available
   - Determines **lifetime**
   - Scopes can be **nested**

* :dfn:`Visibility` of a name

   - Where the name is **actually** available
   - Defined by **visibility rules**
   - **Hidden** |rightarrow| *in scope* but not **directly** visible

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
        M := 123;
        declare
          M : Float;
        begin
          M := 12.34; -- OK
          M := 0;     -- compile error: M is a Float
        end;
        M := 0.0; -- compile error: M is an Integer
        M := 0;   -- OK
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
     M := 123;
     declare
       M : Float;
     begin
       M := 12.34;
       Outer.M := Integer (M);  -- reference "hidden" Integer M
     end;
   end Outer;

------
Quiz
------

.. container:: columns

 .. container:: column

  .. container:: latex_environment footnotesize

   What output does the following code produce? (Assume :code:`Print` prints the current value of its argument)

   .. code:: Ada
      :number-lines: 1

      declare
         M : Integer := 1;
      begin
         M := M + 1;
         declare
            M : Integer := 2;
         begin
            M := M + 2;
            Print (M);
         end;
         Print (M);
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

=========
Aspects
=========

---------
Pragmas
---------

* Originated as a compiler directive for things like

   - Specifying the type of optimization

     .. code:: Ada

        pragma Optimize (Space);

   - Inlining of code

     .. code:: Ada

        pragma Inline (Some_Procedure);

   - Properties (:dfn:`aspects`) of an entity

* Appearance in code

   * Unrecognized pragmas

      .. code:: Ada

         pragma My_Own_Pragma;

      - **No effect**
      - Cause **warning** (standard mode)

   * Must follow correct syntax

      .. code:: Ada

         pragma Page;           -- parameterless
         pragma Optimize (Off); -- with parameter

.. warning:: Malformed pragmas are **illegal**

   :ada:`pragma Illegal One;    -- compile error`

----------------
Aspect Clauses
----------------

* Define **additional** properties of an entity

    - Representation (eg. :ada:`with Pack`)
    - Operations (eg. :code:`Inline`)
    - Can be **standard** or **implementation**-defined

* Usage close to pragmas

    - More **explicit**, **typed**
    - **Recommended** over pragmas

* Syntax

    .. code:: Ada

       with aspect_mark [ => expression]
           {, aspect_mark [ => expression] }

.. note:: Aspect clauses always part of a **declaration**
..
  language_version 2012

--------------------------------
Aspect Clause Example: Objects
--------------------------------

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

..
  language_version 2012

------------------------
Boolean Aspect Clauses
------------------------

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

..
  language_version 2012

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

