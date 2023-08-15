*****************************
Ada Basic Types - Advanced
*****************************

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

=========================
Subtypes - Full Picture
=========================

----------------
Implicit Subtype
----------------

* The declaration

   .. code:: Ada

      type Typ is range L .. R;

* Is short-hand for

   .. code:: Ada

      type <Anon> is new Predefined_Integer_Type;
      subtype Typ is <Anon> range L .. R;

* :ada:`<Anon>` is the :dfn:`Base` type of :ada:`Typ`

    - Accessed with :ada:`Typ'Base`

----------------------------
Implicit Subtype Explanation
----------------------------

.. code:: Ada

   type <Anon> is new Predefined_Integer_Type;
   subtype Typ is <Anon> range L .. R;

* Compiler choses a standard integer type that includes :ada:`L .. R`

   - :ada:`Integer`, :ada:`Short_Integer`, :ada:`Long_Integer`, etc.
   - **Implementation-defined** choice, non portable

* New anonymous type ``<Anon>`` is derived from the predefined type
* ``<Anon>`` inherits the type's operations (``+``, ``-`` ...)
* ``Typ``, subtype of ``<Anon>`` is created with :ada:`range L .. R`
* :ada:`Typ'Base` will return the type ``<Anon>``

-----------------------------
Stand-Alone (Sub)Type Names
-----------------------------

* Denote all the values of the type or subtype

   - Unless explicitly constrained

   .. code:: Ada

      subtype Constrained_Sub is Integer range 0 .. 10;
      subtype Just_A_Rename is Integer;
      X : Just_A_Rename;
      ...
      for I in Constrained_Sub loop
        X := I;
      end loop;

--------------------------------
Subtypes Localize Dependencies
--------------------------------

* Single points of change
* Relationships captured in code
* No subtypes

.. code:: Ada

   type Vector is array (1 .. 12) of Some_Type;

   K : Integer range 0 .. 12 := 0; -- anonymous subtype
   Values : Vector;
   ...
   if K in 1 .. 12 then ...
   for J in Integer range 1 .. 12 loop ...

* Subtypes

.. code:: Ada

   type Counter is range 0 .. 12;
   subtype Index is Counter range 1 .. Counter'Last;
   type Vector is array (Index) of Some_Type;

   K : Counter := 0;
   Values : Vector;
   ...
   if K in Index then ...
   for J in Index loop ...

----------------------------------
Subtypes May Enhance Performance
----------------------------------

* Provides compiler with more information
* Redundant checks can more easily be identified

.. code:: Ada

   subtype Index is Integer range 1 .. Max;
   type Vector is array (Index) of Float;
   K : Index;
   Values : Vector;
   ...
   K := Some_Value;   -- range checked here
   Values (K) := 0.0; -- so no range check needed here

---------------------------------
Subtypes Don't Cause Overloading
---------------------------------

- Illegal code: re-declaration of `F`

   .. code:: Ada

      type A is new Integer;
      subtype B is A;
      function F return A is (0);
      function F return B is (1);

-------------------------------------
Subtypes and Default Initialization
-------------------------------------

.. admonition:: Language Variant

   Ada 2012

* Not allowed: Defaults on new :ada:`type` only

    - :ada:`subtype` is still the same type

* **Note:** Default value may violate subtype constraints

   - Compiler error for static definition
   - :ada:`Constraint_Error` otherwise

.. code:: Ada

   type Tertiary_Switch is (Off, On, Neither)
      with Default_Value => Neither;
   subtype Toggle_Switch is Tertiary_Switch
       range Off .. On;
   Safe : Toggle_Switch := Off;
   Implicit : Toggle_Switch; -- compile error: out of range

----------------------------------------
Attributes Reflect the Underlying Type
----------------------------------------

.. code:: Ada

   type Color is
       (White, Red, Yellow, Green, Blue, Brown, Black);
   subtype Rainbow is Color range Red .. Blue;

* :ada:`T'First` and :ada:`T'Last` respect constraints

   - :ada:`Rainbow'First` |rightarrow| Red *but* :ada:`Color'First` |rightarrow| White
   - :ada:`Rainbow'Last` |rightarrow| Blue *but* :ada:`Color'Last` |rightarrow| Black

* Other attributes reflect base type

   - :ada:`Color'Succ (Blue)` = Brown = :ada:`Rainbow'Succ (Blue)`
   - :ada:`Color'Pos (Blue)` = 4 = :ada:`Rainbow'Pos (Blue)`
   - :ada:`Color'Val (0)` = White = :ada:`Rainbow'Val (0)`

* Assignment must still satisfy target constraints

   .. code:: Ada

      Shade : Color range Red .. Blue := Brown; -- runtime error
      Hue : Rainbow := Rainbow'Succ (Blue);     -- runtime error

------
Quiz
------

.. code:: Ada
    :number-lines: 1

    type T1 is range 0 .. 10;
    function "-" (V : T1) return T1;
    subtype T2 is T1 range 1 .. 9;
    function "-" (V : T2) return T2;

    Obj : T2 := -T2 (1);

Which function is executed at line 6?

A. The one at line 2
B. The one at line 4
C. A predefined ``"-"`` operator for integer types
D. :answer:`None: The code is illegal`

.. container:: animate

    The :ada:`type` is used for the overload profile, and here both :ada:`T1` and :ada:`T2`
    are of type :ada:`T1`, which means line 4 is actually a redeclaration, which is forbidden.

------
Quiz
------

.. code:: Ada

   type T is range 0 .. 10;
   subtype S is T range 1 .. 9;

What is the value of :ada:`S'Succ (S (9))`?

A. 9
B. :answer:`10`
C. None, this fails at runtime
D. None, this does not compile

.. container:: animate

    :ada:`T'Succ` and :ada:`T'Pred` are defined on the :ada:`type`, not the :ada:`subtype`.

------
Quiz
------

.. code:: Ada

    type T is new Integer range 0 .. Integer'Last;
    subtype S is T range 0 .. 10;

    Obj : S;

What is the result of :ada:`Obj := S'Last + 1`?

A. 0
B. 11
C. :answer:`None, this fails at runtime`
D. None, this does not compile

===========
Base Type
===========

-------------
Base Ranges
-------------

* Actual **hardware-supported** numeric type used

   - GNAT makes consistent and predictable choices on all major platforms

* **Predefined** operators

   - Work on full-range

        + **No range checks** on inputs or result
        + Best performance

   - Implementation may use wider registers

        + Intermediate values

* Can be accessed with :ada:`'Base` attribute

   .. code:: Ada

      type Foo is range -30_000 .. 30_000;
      function "+" (Left, Right : Foo'Base) return Foo'Base;

* Base range

    - Signed
    - 8 bits |rightarrow| :ada:`-128 .. 127`
    - 16 bits |rightarrow| :ada:`-32_768 .. 32767`

---------------------------------
Compile-Time Constraint Violation
---------------------------------

* *May* produce **warnings**

    - And compile successfuly

* *May* produce **errors**

    - And fail at compilation

* Requirements for rejection

   - Static value
   - Value not in range of **base** type
   - Compilation is **impossible**

.. code:: Ada

   procedure Test is
      type Some_Integer is range -200 .. 200;
      Object : Some_Integer;
   begin
      Object := 50_000; -- probable error
   end;

-------------------
Range Check Failure
-------------------

* Compile-time rejection

   - Depends on **base** type
   - Selected by the compiler
   - Depends on underlying **hardware**
   - Early error |rightarrow| "Best" case

* Else run-time **exception**

    - Most cases
    - Be happy when compilation failed instead

-----------------------------
Real Base Decimal Precision
-----------------------------

* Real types precision may be **better** than requested
* Example:

   - Available: 6, 12, or 24 digits of precision
   - Type with **8 digits** of precision

      .. code:: Ada

         type My_Type is digits 8;

   - :ada:`My_Type` will have 12 **or** 24 digits of precision

---------------------------------
Floating Point Division By Zero
---------------------------------

* Language-defined do as the machine does

   - If :ada:`T'Machine_Overflows` attribute is :ada:`True` raises :ada:`Constraint_Error`
   - Else :math:`+\infty` / :math:`-\infty`

      + Better performance

* User-defined types always raise :ada:`Constraint_Error`

 .. code:: Ada

    subtype MyFloat is Float range Float'First .. Float'Last;
    type MyFloat is new Float range Float'First .. Float'Last;

-----------------------------------------
Using Equality for Floating Point Types
-----------------------------------------

* Questionable: representation issue

   - Equality |rightarrow| identical bits
   - Approximations |rightarrow| hard to **analyze**, and **not portable**
   - Related to floating-point, not Ada

* Perhaps define your own function

   - Comparison within tolerance (:math:`+\varepsilon` / :math:`-\varepsilon`)

===============
Modular Types
===============

------------------------------------------
Bit Pattern Values and Range Constraints
------------------------------------------

* Binary based assignments possible
* No :ada:`Constraint_Error` when in range
* **Even if** they would be ``<= 0`` as a **signed** integer type

.. code:: Ada

   procedure Demo is
     type Byte is mod 256;  -- 0 .. 255
     B : Byte;
   begin
     B := 2#1000_0000#; -- not a negative value
   end Demo;

---------------------------------
Modular Range Must Be Respected
---------------------------------

.. code:: Ada

   procedure P_Unsigned is
     type Byte is mod 2**8;  -- 0 .. 255
     B : Byte;
     type Signed_Byte is range -128 .. 127;
     SB : Signed_Byte;
   begin
     ...
     B := -256;       -- compile error
     SB := -1;
     B := Byte (SB);  -- runtime error
     ...
   end P_Unsigned;

--------------------------------------
Safely Converting Signed To Unsigned
--------------------------------------

* Conversion may raise :ada:`Constraint_Error`
* Use :ada:`T'Mod` to return :ada:`argument mod T'Modulus`

   - :ada:`Universal_Integer` argument
   - So **any** integer type allowed

  .. code:: Ada

     procedure Test is
       type Byte is mod 2**8;  -- 0 .. 255
       B : Byte;
       type Signed_Byte is range -128 .. 127;
       SB : Signed_Byte;
     begin
       SB := -1;
       B := Byte'Mod (SB);  -- OK (255)

-----------------------
Package **Interfaces**
-----------------------

* **Standard** package
* Integer types with **defined bit length**

   .. code:: Ada

      type My_Base_Integer is new Integer;
      pragma Assert (My_Base_Integer'First = -2**31);
      pragma Assert (My_Base_Integer'Last = 2**31-1);

    - Dealing with hardware registers

* Note: Shorter may not be faster for integer maths

    - Modern 64-bit machines are not efficient at 8-bit maths

.. code:: Ada

   type Integer_8 is range -2**7 .. 2**7-1;
   for Integer_8'Size use 8;
   -- and so on for 16, 32, 64 bit types...

------------------------
Shift/Rotate Functions
------------------------

* In :ada:`Interfaces` package

   - :ada:`Shift_Left`
   - :ada:`Shift_Right`
   - :ada:`Shift_Right_Arithmetic`
   - :ada:`Rotate_Left`
   - etc.

* See RM B.2 - *The Package Interfaces*

---------------------------------
Bit-Oriented Operations Example
---------------------------------

* Assuming :ada:`Unsigned_16` is used

    - 16-bits modular

.. code:: Ada

   with Interfaces;
   use Interfaces;
   ...
   procedure Swap( X : in out Unsigned_16 ) is
   begin
     X := ( Shift_Left(X,8) and 16#FF00# ) or
          ( Shift_Right(X,8) and 16#00FF# );
   end Swap;

---------------------------------
Why No Implicit Shift and Rotate?
---------------------------------

* Arithmetic, logical operators available **implicitly**
* **Why not** :ada:`Shift`, :ada:`Rotate`, etc. ?
* By **excluding** other solutions

   - As functions in **standard** |rightarrow| May **hide** user-defined declarations
   - As new **operators** |rightarrow| New operators for a **single type**
   - As **reserved words** |rightarrow| Not **upward compatible**

-------------------------------------
Shift/Rotate for User-Defined Types
-------------------------------------

* **Must** be modular types
* Approach 1: use :ada:`Interfaces`'s types

    - :ada:`Unsigned_8`, :ada:`Unsigned_16` ...

* Approach 2: derive from :ada:`Interfaces`'s types

   - Operations are **inherited**
   - More on that later

   .. code:: Ada

      type Byte is new Interfaces.Unsigned_8;

* Approach 3: use GNAT's intrinsic

   - Conditions on function name and type representation
   - See GNAT UG 8.11

   .. code:: Ada

      function Shift_Left
        (Value  : T;
        Amount : Natural) return T with Import, Convention => Intrinsic;

------
Quiz
------

.. code:: Ada

    type T is mod 256;
    V : T := 255;

Which statement(s) is(are) legal?

A. :answermono:`V := V + 1`
B. :answermono:`V := 16#ff#`
C. ``V := 256``
D. :answermono:`V := 255 + 1`

------
Quiz
------

.. code:: Ada

    with Interfaces; use Interfaces;

    type T1 is new Unsigned_8;
    V1 : T1 := 255;

    type T2 is mod 256;
    V2 : T2 := 255;

Which statement(s) is(are) legal?

A. :answermono:`V1 := Rotate_Left (V1, 1)`
B. ``V1 := Positive'First``
C. :answermono:`V2 := 1 and V2`
D. ``V2 := Rotate_Left (V2, 1)``
E. ``V2 := T2'Mod (2.0)``

=======================
Representation Values
=======================

-----------------------------------
Enumeration Representation Values
-----------------------------------

* Numeric **representation** of enumerals

    - Position, unless redefined
    - Redefinition syntax

      .. code:: Ada

         type Enum_T is (Able, Baker, Charlie, Dog, Easy, Fox);
         for Enum_T use (1, 2, 4, 8, Easy => 16, Fox => 32);

* No manipulation *in language standard*

   - Standard is **logical** ordering
   - Ignores **representation** value

* Still accessible

   - **Unchecked** conversion
   - **Implementation**-defined facility

      + Ada 2022 attributes :ada:`T'Enum_Rep`, :ada:`T'Enum_Val`

-----------------------------------------
Order Attributes For All Discrete Types
-----------------------------------------

* **All discrete** types, mostly useful for enumerated types
* :ada:`T'Pos (Input)`

   - "Logical position number" of :ada:`Input`

* :ada:`T'Val (Input)`

   - Converts "logical position number" to :ada:`T`

.. code:: Ada

   type Days is ( Sun, Mon, Tue, Wed, Thu, Fri, Sat ); -- 0 .. 6
   Today    : Days := Some_Value;
   Position : Integer;
   ...
   Position := Days'Pos( Today );
   ...
   Get( Position );
   Today := Days'Val( Position );

.. container:: speakernote

   Val/pos compared to value/image - same number of characters

------
Quiz
------

.. code:: Ada

    type T is (Left, Top, Right, Bottom);
    V : T := Left;

Which of the following proposition(s) are true?

A. ``T'Value (V) = 1``
B. :answermono:`T'Pos (V) = 0`
C. ``T'Image (T'Pos (V)) = Left``
D. ``T'Val (T'Pos (V) - 1) = Bottom``

=================
Character Types
=================

----------------------------------
Language-Defined Character Types
----------------------------------

* :ada:`Character`

   - 8-bit Latin-1
   - Base element of :ada:`String`
   - Uses attributes :ada:`'Image` / :ada:`'Value`

* :ada:`Wide_Character`

   - 16-bit Unicode
   - Base element of :ada:`Wide_Strings`
   - Uses attributes :ada:`'Wide_Image` / :ada:`'Wide_Value`

* :ada:`Wide_Wide_Character`

   - 32-bit Unicode
   - Base element of :ada:`Wide_Wide_Strings`
   - Uses attributes :ada:`'Wide_Wide_Image` / :ada:`'Wide_Wide_Value`

-----------------------------
Character Oriented Packages
-----------------------------

* Language-defined
* :ada:`Ada.Characters.Handling`

   - Classification
   - Conversion

* :ada:`Ada.Characters.Latin_1`

   - Characters as constants

* See RM Annex A for details

-----------------------------------------
`Ada.Characters.Latin_1` Sample Content
-----------------------------------------

.. code:: Ada

   package Ada.Characters.Latin_1 is
     NUL : constant Character := Character'Val (0);
     ...
     LF  : constant Character := Character'Val (10);
     VT  : constant Character := Character'Val (11);
     FF  : constant Character := Character'Val (12);
     CR  : constant Character := Character'Val (13);
     ...
     Commercial_At  : constant Character := '@';  -- Character'Val(64)
     ...
     LC_A : constant Character := 'a';  -- Character'Val (97)
     LC_B : constant Character := 'b';  -- Character'Val (98)
     ...
     Inverted_Exclamation : constant Character := Character'Val (161);
     Cent_Sign            : constant Character := Character'Val (162);
   ...
     LC_Y_Diaeresis       : constant Character := Character'Val (255);
   end Ada.Characters.Latin_1;

----------------------------------------
Ada.Characters.Handling Sample Content
----------------------------------------

.. code:: Ada

   package Ada.Characters.Handling is
     function Is_Control           (Item : Character) return Boolean;
     function Is_Graphic           (Item : Character) return Boolean;
     function Is_Letter            (Item : Character) return Boolean;
     function Is_Lower             (Item : Character) return Boolean;
     function Is_Upper             (Item : Character) return Boolean;
     function Is_Basic             (Item : Character) return Boolean;
     function Is_Digit             (Item : Character) return Boolean;
     function Is_Decimal_Digit     (Item : Character) return Boolean renames Is_Digit;
     function Is_Hexadecimal_Digit (Item : Character) return Boolean;
     function Is_Alphanumeric      (Item : Character) return Boolean;
     function Is_Special           (Item : Character) return Boolean;
     function To_Lower (Item : Character) return Character;
     function To_Upper (Item : Character) return Character;
     function To_Basic (Item : Character) return Character;
     function To_Lower (Item : String) return String;
     function To_Upper (Item : String) return String;
     function To_Basic (Item : String) return String;
   ...
   end Ada.Characters.Handling;

------
Quiz
------

.. include:: quiz/user_defined_character/quiz.rst

------
Quiz
------

.. code:: Ada

    with Ada.Characters.Latin_1;
    use Ada.Characters.Latin_1;
    with Ada.Characters.Handling;
    use Ada.Characters.Handling;

Which of the following proposition(s) are true?

A. ``NUL = 0``
B. ``NUL = '\0'``
C. :answermono:`Character'Pos (NUL) = 0`
D. :answermono:`Is_Control (NUL)`
