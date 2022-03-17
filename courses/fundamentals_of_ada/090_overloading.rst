*************
Overloading
*************

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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

--------------
Introduction
--------------

* Overloading is the use of an already existing name to define a **new** entity
* Historically, only done as part of the language **implementation**

   - Eg. on operators
   - Float vs integer vs pointers arithmetic

* Several languages allow **user-defined** overloading

   - C++
   - Python (limited to operators)
   - Haskell

----------------------
Visibility and Scope
----------------------

* Overloading is **not** re-declaration
* Both entities **share** the name

   - No hiding
   - Compiler performs **name resolution**

* Allowed to be declared in the **same scope**

   - Remember this is forbidden for "usual" declarations

------------------------------
Overloadable Entities In Ada
------------------------------

* Identifiers for subprograms

   - Both procedure and function names

* Identifiers for enumeration values (enumerals)
* Language-defined operators for functions

.. code:: Ada

   procedure Put (Str : in String);
   procedure Put (C : in Complex);
   function Max (Left, Right : Integer) return Integer;
   function Max (Left, Right : Float)   return Float;
   function "+" (Left, Right : Rational) return Rational;
   function "+" (Left, Right : Complex)  return Complex;
   function "*" (Left : Natural; Right : Character)
         return String;

---------------------------------------
Function Operator Overloading Example
---------------------------------------

.. code:: Ada

   --  User-defined overloading
   function "+" (L,R : Complex) return Complex is
   begin
     return (L.Real_Part + R.Real_Part,
             L.Imaginary + R.Imaginary);
   end "+";

   A, B, C : Complex;
   I, J, K : Integer;

   I := J + K; -- overloaded operator (predefined)
   A := B + C; -- overloaded operator (user-defined)

----------------------------------
Benefits and Risk of Overloading
----------------------------------

* Management of the name space

   - Support for abstraction
   - Linker will not simply take the first match and apply it globally

* Safe: compiler will reject ambiguous calls
* Sensible names are the programmer's job

   .. code:: Ada

      function "+" ( L, R : Integer ) return String is
      begin
         return Integer'Image ( L - R );
      end "+";

=========================
Enumerals and Operators
=========================

----------
Examples
----------

.. include:: examples/090_overloading/enumerals_and_operators.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/090_overloading.html#enumerals-and-operators`

-----------------------
Overloading Enumerals
-----------------------

* Each is treated as if a function name (identifier)
* Thus same rules as for function identifier overloading

.. code:: Ada

   type Stop_Light is (Red, Yellow, Green);
   type Colors is (Red, Blue, Green);
   Shade : Colors := Red;
   Current_Value : Stop_Light := Red;

.. container:: speakernote

   Red and Green are overloaded (but context helps to resolve)

-------------------------------
Overloadable Operator Symbols
-------------------------------

* Only those defined by the language already

   - Users cannot introduce new operator symbols

* Note that assignment (:=) is not an operator
* Operators (in precedence order)

  :Logicals: and, or, xor
  :Relationals: ``<``, ``<=``, ``=``, ``>=``, ``>``
  :Unary: ``+``, ``-``
  :Binary: ``+``, ``-``, ``&``
  :Multiplying: ``*``, ``/``, ``mod``, ``rem``
  :Highest precedence : ``**``, ``abs``, ``not``

-------------------------------------
Parameters for Overloaded Operators
-------------------------------------

* Must not change syntax of calls

   - Number of parameters must remain same (unary, binary...)
   - No default expressions allowed for operators

* Infix calls use positional parameter associations

   - Left actual goes to first formal, right actual goes to second formal
   - Definition

      .. code:: Ada

         function "*" (Left, Right : Integer) return Integer;

   - Usage

      .. code:: Ada

         X := 2 * 3;

* Named parameter associations allowed but ugly

   - Requires prefix notion for call

   .. code:: Ada

      X := "*" ( Left => 2, Right => 3 );

=================
Call Resolution
=================

----------
Examples
----------

.. include:: examples/090_overloading/call_resolution.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/090_overloading.html#call-resolution`

-----------------
Call Resolution
-----------------

* Compilers must reject ambiguous calls
* Resolution is based on the calling context

   - Compiler attempts to find a matching **profile**
   - Based on **Parameter** and **Result** Type

* Overloading is not re-definition, or hiding

   - More than one matching profile is ambiguous

.. code:: Ada

   type Complex is ...
   function "+" (L, R : Complex) return Complex;
   A, B : Complex := some_value;
   C : Complex := A + B;
   D : Real := A + B;  -- illegal!
   E : Real := 1.0 + 2.0;

-------------------------
Profile Components Used
-------------------------

* Significant components appear in the call itself

   - **Number** of parameters
   - **Order** of parameters
   - **Base type** of parameters
   - **Result** type (for functions)

* Insignificant components might not appear at call

   - Formal parameter **names** are optional
   - Formal parameter **modes** never appear
   - Formal parameter **subtypes** never appear
   - **Default** expressions never appear

   .. code:: Ada

      Display (X);
      Display (Foo => X);
      Display (Foo => X, Bar => Y);

-------------------------------
Manually Disambiguating Calls
-------------------------------

* Qualification can be used
* Named parameter association can be used

   - Unless name is ambiguous

.. code:: Ada

   type Stop_Light is (Red, Yellow, Green);
   type Colors is (Red, Blue, Green);
   procedure Put (Light : in Stop_Light);
   procedure Put (Shade : in Colors);

   Put (Red);  -- ambiguous call
   Put (Yellow);  -- not ambiguous: only 1 Yellow
   Put (Colors'(Red)); -- using type to distinguish
   Put (Light => Green); -- using profile to distinguish

---------------------
Overloading Example
---------------------

.. code:: Ada

   function "+" (Left : Position; Right : Offset)
     return Position is
   begin
      return Position'( Left.Row + Right.Row, Left.Column + Right.Col);
   end "+";

   function Acceptable (P : Position) return Boolean;
   type Positions is array (Moves range <>) of Position;

   function Next (Current : Position) return Positions is
     Result : Positions (Moves range 1 .. 4);
     Count  : Moves := 0;
     Test   : Position;
   begin
     for K in Offsets'Range loop
       Test := Current + Offsets(K);
       if Acceptable (Test) then
         Count := Count + 1;
         Result (Count) := Test;
       end if;
     end loop;
     return Result (1 .. Count);
   end Next;

.. container:: speakernote

   If Count is 0, result is a null range

------
Quiz
------

.. code:: Ada

   type Vertical_T is (Top, Middle, Bottom);
   type Horizontal_T is (Left, Middle, Right);
   function "*" (H : Horizontal_T; V : Vertical_T) return Positive;
   function "*" (V : Vertical_T; H : Horizontal_T) return Positive;
   P : Positive;

Which statement is not legal?

   A. ``P := Horizontal_T'(Middle) * Middle;``
   B. ``P := Top * Right;``
   C. ``P := "*" (Middle, Top);``
   D. :answermono:`P := "*" (H => Middle, V => Top);`

.. container:: animate

   Explanations

   A. Qualifying one parameter resolves ambiguity
   B. No overloaded names
   C. Use of :ada:`Top` resolves ambiguity
   D. When overloading subprogram names, best to not just switch the order of parameters

=======================
User-Defined Equality
=======================

----------
Examples
----------

.. include:: examples/090_overloading/user_defined_equality.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/090_overloading.html#user-defined-equality`

-----------------------
User-Defined Equality
-----------------------

* Allowed like any other operator

   - Must remain a binary operator

* Typically declared as :ada:`return Boolean`
* Hard to do correctly for composed types

    - Especially **user-defined** types
    - Issue of *Composition of equality*

========
Lab
========

.. include:: labs/090_overloading.lab.rst

=========
Summary
=========

---------
Summary
---------

* Ada allows user-defined overloading

   - Identifiers and operator symbols

* Benefits easily outweigh danger of senseless names

   - Can have nonsensical names without overloading

* Compiler rejects ambiguous calls
* Resolution is based on the calling context

   - *Parameter and Result Type Profile*

* Calling context is those items present at point of call

   - Thus modes etc. don't affect overload resolution

* User-defined equality is allowed

   - But is tricky
