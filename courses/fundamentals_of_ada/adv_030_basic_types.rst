*****************************
Ada Basic Types - Advanced
*****************************

.. |rightarrow| replace:: :math:`\rightarrow`

.. role:: ada(code)
   :language: ada

.. role:: C(code)
   :language: C

.. role:: cpp(code)
   :language: C++

===========
Base Type
===========

----------------
Implicit Subtype
----------------

* The declaration

   .. code:: Ada

      type Typ is range L .. R;

* Is short-hand for

   .. code:: Ada

      type Typ is new Predefined_Integer_Type;
      subtype Sub is Typ range L .. R;

----------------------------
Implicit Subtype Explanation
----------------------------

.. code:: Ada

   type Typ is new Predefined-Integer-Type;
   subtype Sub is Typ range L .. R;

* Compiler choses a standard integer type that includes L .. R

   - :ada:`Integer`, :ada:`Short_Integer`, :ada:`Long_Integer`, etc.
   - **Implementation-defined** choice, non portable

* New anonymous type `Typ` is derived from the predefined type
* `Typ` inherits the type's operations (``+``, ``-`` ...)
* `Sub`, subtype of `Typ` is created with range L .. R
* :ada:`Sub'Base` will return the type `Typ`

-------------
Base Ranges
-------------

* Actual **hardware-supported** numeric type used

   - GNAT makes consistent and predictable choices on all major platforms.

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

   procedure Unsigned is
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
   end Unsigned;

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
      type Half_Word is new Interfaces.Unsigned_16;
      type Word is new Interfaces.Unsigned_32;

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

      + GNAT attribute :ada:`T'Enum_Rep`

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

============
Subtypes
============

-----------------------------
Stand-Alone (Sub)Type Names
-----------------------------

* Denote all the values of the type or subtype

   - Unless explicitly constrained

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

