*************
Basic Types
*************

.. |rightarrow| replace:: :math:`\rightarrow`

================
Introduction
================

----------------
Ada Type Model
----------------

* Statically Typed

   - Each object is permanently declared to be of one type

* Strongly Typed

   - Based on types' names
   - Compiler enforces appropriate manipulation and values
   - Objects of "closely-related" types may be converted
   - Conversions between unrelated types are not checked

* Many types are language-defined
* Users extend the language by defining new types

   - Optional, could make everything a single (numeric) type if you wish...

---------------
Strong Typing
---------------

* Definition of **type**

   - Set of applicable values
   - Set of applicable operations on objects of the type

* The compiler enforces your application model

   - Allowed values
   - Allowed operations

* Making the computer work for you

   - Bookkeeping is what it does best!
   - Allows us to focus on "the hard stuff"

----------------------
A Little Terminology
----------------------

* Type names are introduced by declarations

   .. code:: Ada

      type name is type-definition;
 
* A type-definition defines the type's structure

   - Characteristics and operations
   - The "kind" of the type itself

      .. code:: Ada

         type typemark is digits 12; -- floating-point
         type typemark is range -200 .. 200; -- signed integer
         type typemark is mod 256; -- unsigned integer

-------------------------
Ada Uses "Named Typing"
-------------------------

* Types are differentiated by name, not structure
* Hence identical structures do not convey interoperability

   .. code:: Ada

      type Yen is range 0 .. 100_000_000;
      type Ruble is range 0 .. 100_000_000;
      Mine : Yen;
      Yours : Ruble;
      ...
      Mine := Yours; -- not legal
 
---------------------
Categories of Types
---------------------

.. image:: ../../images/types_tree.png

--------------
Scalar Types
--------------

* Logically indivisible

   - Have no components

* Ordered

   - Relational operators ``<``, ``>``, ``=``, ``/=``, and so on are defined

* Support common "attributes" (language-defined functions applied to types)
* Flavors

   - Discrete Types

      - Numeric Types
      - Enumeration Types

   - Real Types

----------------
Discrete Types
----------------

* Define individual ("discrete") values

   - 1,2,3,4 ...
   - Red, Yellow, Green

* Numeric Types

   - Signed integer types
   - Modular integer types

      * Unsigned values with wrap-around semantics

* Enumeration types

   - An enumerated list of values

========================
Discrete Numeric Types
========================

-----------
Examples
-----------

.. include:: examples/030_basic_types/discrete_numeric_types.rst

----------------------
Signed Integer Types
----------------------

* Define a range of signed whole numbers

   - Symmetric about zero (-0 = +0)

* Syntax

   .. code:: Ada

      type <identifier> is range  <lower> .. <upper>;
 
* Numeric operators are automatically defined

   .. code:: Ada

      -- 12-bit device
      type Analog_Conversions is range 0 .. 4095;
      Count : Analog_Conversions;
      ...
      begin
         ...
         Count := Count + 1;
         ...
      end;
 
--------------------------------
Specifying Integer Type Bounds
--------------------------------

* Must be static

   - Compiler assigns a hardware-supported integer type

* Insupportable ranges are rejected at compile-time

--------------------------
Predefined Integer Types
--------------------------

* A predefined signed integer type exists

   - Name (confusingly) is `integer`
   - Range is implementation-defined but at least 16 bits wide

* Other predefined integer types are also likely

   - `Long_Integer`, `Short_Integer`, etc.
   - Relationship of sizes to Integer's size is guaranteed

      + Size of `Short_Integer` ``<=`` Size of `Integer` ``<=`` Size of `Long_Integer`

* Best to avoid predefined types

   - Portability may suffer
   - Sometimes difficult to avoid

--------------------------------
Operators for Any Integer Type
--------------------------------

* Ordered by increasing precedence

   :relational operator: ``= | /= | < | <= | > | >=``
   :binary adding operator: ``+ | -``
   :unary adding operator: ``+ | -``
   :multiplying operator: ``* | / | mod | rem``
   :highest precedence operator: ``** | abs``

* Note on exponentiation

   - Requested power must be a non-negative `Integer` value, otherwise you're asking for a floating-point (real) result

* Division by zero raises `Constraint_Error`

-------------
Base Ranges
-------------

* The hardware's numeric types actually used
* Used by predefined operators for signed integers

   - No range checks on inputs or result to affect performance
   - Implementation can use wider registers for intermediate values

* You can refer to them too

   .. code:: Ada

      type Foo is range -30_000 .. 30_000;
      function "+" (Left, Right : Foo'Base) return Foo'Base;
 
* Base range for

   - 8 bits |rightarrow| `-128 .. 127`
   - 16 bits |rightarrow| `-32_768 .. 32767`

---------------------------------------------
Compile-Time Constraint Violation Detection
---------------------------------------------

* May generate warnings and produce executable

* Allowed to be treated as errors 

   - Hence program may be rejected at compile-time

* Requirements for rejection

   - Value is static
   - Value exceeds base range of corresponding type

.. code:: Ada

   procedure Test is
      type Some_Integer is range -200 .. 200;
      Object : Some_Integer;
   begin
      Object := 50_000; -- probably rejected by compiler
   end;

-------------------------
Either Way You Find Out
-------------------------

* Compile-time rejection depends on base range

   - Selected by the compiler
   - Depends on underlying hardware

* Run-time exception otherwise
* So assume an exception will be the result and be happy when the compiler catches them beforehand

--------------------------------------
Values of Integer Types Can Overflow
--------------------------------------

* A result of finite binary representation

.. code:: Ada

   K : Integer := Integer'Last;
   ...
   K := K + 1;
      
    2#01111111111111111111111111111111# - (2**32)-1

   +                                 1

   ===================================
   2#100000000000000000000000000000000#

   ( = -2,147,483,64810 )
 
-----------------------------------
What Happens On Integer Overflow?
-----------------------------------

* Ada

   - Language-defined exception `Constraint_Error`
   - Exception means numerical analysis was incorrect

* Java

   - Silently wraps around (that's what the hardware does)

* C/C++

   - Undefined behavior (but silent wrap-around is typical)

------------------------------------
Overflow Detection At Compile Time
------------------------------------

* Allowed by Ada
* Code to raise `Constraint_Error` replaces code that causes `Constraint_Error`

   - Only for language-defined operations since no side-effects

   .. code:: Ada

      procedure Compute is
         K : Integer := Integer'Last;
      begin
         K := K + 1; -- generated code raises Constraint_Error
         ...
      end Compute;
 
---------------
Modular Types
---------------

* Another form of integer type
* Support unsigned values and additional operations
* Syntax

   .. code:: Ada

      type <identifier> is mod <modulus>;
 
* Modulus must be static
* Resulting range is  0 .. modulus-1

   .. code:: Ada

      type Unsigned_Word is mod 2**16; -- 16 bits,  0 .. 65_535
      type Byte is mod 256;            -- 8 bits, 0 .. 255
 
------------------------
Modular Type Semantics
------------------------

* Predefined operators as for any integer type

   - Addition, subtraction, et cetera

* But values "wrap-around" from 0 to modulus-1

   - Like other languages' unsigned types
   - Mathematical operators
   - Attributes 'Pred and 'Succ

* Additional bit-oriented operations are defined

   - `and`, `or`, `xor`, `not`
   - Treat values as bit-sequences

------------------------------------------
Bit Pattern Values and Range Constraints
------------------------------------------

* Bit pattern assignments will not violate constraints if they represent values in the range, even if they represent negative numbers in a signed integer type

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

* Since conversion may raise `Constraint_Error`
* Attribute ``T'Mod`` returns argument mod ``T'Modulus``

   - Argument is a `Universal_Integer` so allows any integer type

   .. code:: Ada

      procedure Test is
        type Byte is mod 2**8;  -- 0 .. 255  
        B : Byte;
        type Signed_Byte is range -128 .. 127;
        SB : Signed_Byte;
      begin
        SB := -1;
        B := Byte'Mod (SB);  -- OK (255)
 
--------------------------
Predefined Modular Types
--------------------------

* Declared in package Interfaces

   - Not automatically available like the language-defined types

* Represent machine numeric types
* Types' characteristics vary with hardware but have common name format

   - `Unsigned_n`
   - `Integer_n`

.. code:: Ada

   type Integer_8 is range -2 ** 7 .. 2 ** 7 - 1;
   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   ...
   type Unsigned_8 is mod 2 ** 8;
   type Unsigned_16 is mod 2 ** 16;
 
------------------------
Shift/Rotate Functions
------------------------

* Defined just for the modular types in `Interfaces`

   - `Shift_Left`
   - `Shift_Right`
   - `Shift_Right_Arithmetic`
   - `Rotate_Left`
   - `Rotate_Right`
   - others...

---------------------------------
Bit-Oriented Operations Example
---------------------------------

* Assume an unsigned 16-bit type is provided

.. code:: Ada

   with Interfaces;
   use Interfaces;
   ... 
   procedure Swap( X : in out Unsigned_16 ) is
   begin
     X := ( Shift_Left(X,8) and 16#FF00# ) or
          ( Shift_Right(X,8) and 16#00FF# );
   end Swap;
 
--------------------------------------------
Why Not Shift and Rotate For All Modulars?
--------------------------------------------

* Designers considered making them available

   - Like arithmetic and logical operators

* Limitation to types in `Interfaces` is for good reason

   - Non-overloadable user-defined declarations (e.g. variables) with the same identifiers would be hidden
   - If ``Shift`` and ``Rotate`` were made into operators, hiding would not be a problem but then new operators would have had to be defined by the language (a potentially big deal)
   - ``Shift`` and ``Rotate`` could have been made into reserved words but that would introduce an upward incompatibility

-------------------------------------
Shift/Rotate for User-Defined Types
-------------------------------------

* Modular types, of course
* Approach 1: use types in `Interfaces` directly

   .. code:: Ada

      with Interfaces;  use Interfaces;
      with Ada.Text_IO;
      with ...
 
* Approach 2: derive types from those in `Interfaces`

   - All primitive operations are inherited

   .. code:: Ada

      type Byte is new Interfaces.Unsigned_8;
      type Half_Word is new Interfaces.Unsigned_16;
      type Word is new Interfaces.Unsigned_32;
 
---------------------------------------------
Integer Type (Signed and Modular) Literals 
---------------------------------------------

* Must not contain a fractional part
* No silent promotion/demotion done by compiler

.. code:: Ada

   type Event_Counter is range 0 .. 40_000; -- integer type
   OK : Event_Counter := 0; -- legal
   Bad : Event_Counter := 0.0 ; -- compile error

------------
Attributes
------------

* Built-in functions associated with types

   - Supply a value
   - Some take input parameters

* Some are language-defined

   - Summarized in RM K.2 "Language-Defined Attributes"

* May be implementation-defined
* Syntax for calls

   - Type-Name ' Attribute-Name 

      .. code:: Ada

         I := Integer'Last;
 
   - Type-Name ' Attribute-Name ( input-value )

      .. code:: Ada

         E1 := Some_Type'Succ ( E2 );
 
-----------------------------------
String Attributes For All Scalars
-----------------------------------

* `T'Image( input )`

   - Converts given internal value to a `String` value
   - Input value must be of type `T`

* `T'Value( input )`

   - Converts given `String` value to an internal value of type `T`
   - Input must be of type `String`

.. code:: Ada

   Number : Integer := 12345;
   Input  : String( 1 .. N );
   ...
   Put_Line( Integer'Image(Number) );
   ...
   Get( Input );
   Number := Integer'Value( Input );
 
----------------------------------
Range Attributes For All Scalars
----------------------------------

* `T'First`

  - Yields the first (least) value of type `T`

* `T'Last`

  - Yields the last (greatest) value of type `T`

* `T'Range`

  - Is a shorthand for ``T'First .. T'Last``

.. code:: Ada
   
   type Signed_T is range -100 .. 100;
   Smallest : Signed_T := Signed_T'First; -- -100
   Largest  : Signed_T := Signed_T'Last;  -- 100
 
-------------------------------------
Neighbor Attributes For All Scalars
-------------------------------------

* `T'Pred( input )`

   - Yields the predecessor of specified value
   - Input must be of type `T`

* `T'Succ ( input )`

   - Yields the successor of specified value
   - Input must be of type `T`

.. code:: Ada

   type Signed_T is range -128 .. 127;
   type Unsigned_T is mod 256;
   Signed   : Signed_T := 0;
   Unsigned : Unsigned_T := 0;
   ...
   Signed := Signed'Succ( Signed );
   ...
   Unsigned := Unsigned'Pred( Unsigned );
 
------------------------------------
Min/Max Attributes For All Scalars
------------------------------------

* `T'Min`

  - Yields the lesser of two values of type T`

* `T'Max`

  - Yields the greater of two values of type T`

.. code:: Ada
   
   Safe_Lower : constant := 10;
   Safe_Upper : constant := 30;
   C : Integer := 15;
   ...
   C := Integer'Max (Safe_Lower, C - 1);
   ...
   C := Integer'Min (Safe_Upper, C + 1);
 
.. container:: speakernote

   First one says we can't decrement below 10
   Second one says we can't increment above 30

============================
Discrete Enumeration Types
============================
 
-----------
Examples
-----------

.. include:: examples/030_basic_types/discrete_enumeration_types.rst

-------------------
Enumeration Types
-------------------

* Represent an enumeration of logical values
* Syntax

   .. code:: Ada

      type <identifier> is ( <identifier-list> ) ;
 
* Literals

   - Distinct, ordered
   - Can be overloaded between types

   .. code:: Ada

      type Colors is (Red, Orange, Yellow, Green, Blue, Violet);
      type Stop_Light is (Red, Yellow, Green);
      ...
      Shade : Colors := Red;
      Light : Stop_Light := Red;
 
-----------------------------
Enumeration Type Operations
-----------------------------

* Assignment, relationals
* Cannot be treated as numeric quantities

   - Can achieve the effect via attributes

.. code:: Ada

   type Directions is ( North, South, East, West );
   type Days is ( Mon, Tue, Wed, Thu, Fri, Sat, Sun );
   Heading : Directions;
   Today, Tomorrow : Days;
   ...
   Today := Mon;
   Today := North; -- compile error
   Heading := South;
   Heading := East + 1; -- compile error
   if Today < Tomorrow then ...
 
-----------------
Character Types
-----------------

* Literals

   - Enclosed in single quotes
   - Case-sensitive

* Declared as enumeration types

   - An enumeration type with at least one character enumeral

* Language defines various character types
* Users can define their own character types

   .. code:: Ada

      type EBCDIC is ( nul, ..., 'a' , ..., 'A', ..., del );
      Control : EBCDIC := 'A';
      Nullo : EBCDIC := nul;
 
----------------------------------
Language-Defined Character Types
----------------------------------

* `Character`

   - 8-bit (Latin-1) character set
   - Used in language-defined type `String`

* `Wide_Character`

   - 16-bit Unicode
   - Used in language-defined type `Wide_String`

* `Wide_Wide_Character`

   - 32-bit Unicode
   - Used in language-defined type `Wide_Wide_String`

-----------------------------
Character Oriented Packages
-----------------------------

* Language-defined
* `Ada.Characters.Handling`

   - Classification and conversion functions for `Character` data 

* `Ada.Characters.Latin_1`

   - Declares a set of constants

* See Annex A for details

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
 
-------------------------------
Language-Defined Type Boolean
-------------------------------

* An enumeration type

   .. code:: Ada

      type Boolean is ( False, True );
 
* Supports assignment, relationals, attributes

   .. code:: Ada

      A : Boolean;
      Counter : Integer;
      ...
      A := Counter = 22;  -- is Counter 22?
 
* Includes logical operators: `and`, `or`, `xor`, `not`

   .. code:: Ada

      A := B or ( not C ); -- assuming A, B, C boolean
 
------------------------------------
Why Boolean Isn't Just An Integer?
------------------------------------

.. container:: columns

 .. container:: column
  
    * Consider a real-life example of an error

       - The HETE-2 satellite attitude control system software (ACS)
       - Written in C

    * ACS includes control of four "solar paddles" deployed after launch

 .. container:: column
  
    .. image:: ../../images/hete-2_satellite.jpeg
    
------------------------------------
Why Boolean Isn't Just An Integer!
------------------------------------

* HETE-2 ACS Solar Paddle State allowed callers to test whether all paddles were or were not deployed
* Initial approach was a boolean variable

   .. code:: C++

      if (rom->paddles_deployed == 1) 
        use_deployed_inertia_matrix();
      else
        use_stowed_inertia_matrix();
 
* Later, each paddle became monitored individually, so `paddles_deployed` became a 4-bit value (0 : all stowed, 0xF : all deployed)
* But not all client code (above) was changed!

   - Now `use_deployed_inertia_matrix` is called only when 1 specific paddle is deployed!

.. container:: speakernote

   Of course, it should have been a boolean function anyway, so that the change to four-bits would only matter in that one place (where the function is implemented).

---------------------------------------
Boolean Operators' Operand Evaluation
---------------------------------------

* Order not specified by the language

   - Unlike, for example, C which is always left before right

* Usually does not matter

   - Sometimes critical to avoid errors!

      + Dividing by zero
      + Dereferencing null pointers
      + Using some inappropriate value in a user-defined operation

      .. code:: Ada

         if Divisor /= 0 and K / Divisor = Max then ... -- Problem!
 
-----------------------------
Short-Circuit Control Forms
-----------------------------

* Boolean operators with fixed evaluation order
* Left operand is always evaluated first
* Right operand only evaluated if necessary

   - Two forms
   - `and then` if left operand is False, doesn't evaluate right

      .. code:: Ada

         if Divisor /= 0 and then K / Divisor = Max then ...
 
   - `or else` if left operand is True, doesn't evaluate right

      .. code:: Ada

         if Divisor = 0 or else K / Divisor = Max then ...
 
-----------------------------------
Enumeration Representation Values
-----------------------------------

.. code:: Ada

   type Enum_T is (Able, Baker, Charlie, Dog, Easy, Fox);
   for Enum_T use (1, 2, 4, 8, Easy => 16, Fox => 32);
 
* Not provided by `'Pos` and `'Val`

   - They work in terms of logical ordering
   - Regardless of the actual value used to represent enumerals

* Are accessible, but not by built-in function

   - Unchecked conversion
   - Implementation-defined facility

      .. code:: Ada

         -- GNAT-specific function
         function T'Enum_Rep (Arg : T'Base)
             return Universal_Integer;
 
   - Are same as position numbers unless overridden
   - Thus users can rely on consistency when necessary

-----------------------------------------
Order Attributes For All Discrete Types
-----------------------------------------

* Valid for all discrete types, but most useful for enumerated types

* `T'Pos ( input )`

   - Yields the "logical position number" of specified value
   - Input must be a value of type `T`

* `T'Val ( input )`

   - Converts specified position number to a value of type `T`

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
Real Types
============

-----------
Examples
-----------

.. include:: examples/030_basic_types/real_types.rst

------------
Real Types
------------

* Define approximations to continuous values

   - 1.0, 1.1, 1.11, 1.111 ... 2.0, ...
   - Values are approximations since digital hardware is finite

* Come in two flavors

   - Floating-point types

      + Radix-point position depends upon value's magnitude

   - Fixed-point types

      + Radix-point position is not dependent upon the magnitude

* Fixed-point types come in two flavors as well

   - Ordinary fixed-point types
   - Decimal fixed-point types

* Focus on floating-point types here

------------------------------------------
Real Type (Floating and Fixed) Literals 
------------------------------------------

* Must contain a fractional part
* No silent promotion/demotion done by compiler

.. code:: Ada

   type Phase is digits 8; -- floating-point
   OK : Phase := 0.0;
   Bad : Phase := 0 ; -- compile error
 
--------------------------------
Declaring Floating Point Types
--------------------------------

* Syntax

   .. code:: Ada
          
      type <identifier> is
          digits <expression> [range constraint];
 
   - Digits is the minimum number of significant decimal digits required

* Automatic representation choice

   - Compiler chooses closest implementation available
   - May be more accurate, but not less
   - If not available, declaration is rejected

---------------------------------
Predefined Floating Point Types
---------------------------------

* Type `Float`

   - Number of digits is implementation-defined
   - At least 6 digits if the hardware can support 6 or more

* Additional implementation-defined types may exist

   - `Long_Float`, if present, will support at least 11 digits (if...)

* Use when you don't care what you get

   - Or when 6/11/etc. digits will always be enough

* Best to avoid predefined types

   - Portability may suffer
   - Easier to avoid than Integer
   - Assume type `Real` is user-defined in class examples

------------------------
Base Decimal Precision
------------------------

* That which is actually provided

   - Based on type declaration
   - Perhaps beyond requested number of digits

* Referenced via attribute `'Base`

   - Example: Compiler supports 6, 12, or 24 digits of precision
   - User requests 8 bits of precision

   .. code:: Ada

      type my_type is digits 8;
 
   - Compiler can create objects using 12 **or** 24 digits of precision

-------------------------------
Floating Point Type Operators
-------------------------------

* Ordered by increasing precedence

   :relational operator: ``= | /= | < | >= | > | >=``
   :binary adding operator: ``+ | -``
   :unary adding operator: ``+ | -``
   :multiplying operator: ``* | /``
   :highest precedence operator: ``** | abs``

* Note on exponentiation

   - Requested power must be an integer value (positive or negative), otherwise you're asking for roots

      + `X**0.5` |rightarrow| `sqrt(x)`

---------------------------------
Floating Point Division By Zero
---------------------------------

* Language-defined types do what the machine does

   - Raises exception if attribute `T'Machine_Overflows` is True

      + For some type `T`, e.g., `Float`
      + `Constraint_Error`

   - Generates infinities otherwise
   - Best performance

* If that's not what you want, define a constrained range:

   .. code:: Ada

      subtype MyFloat is Float range Float'First .. Float'Last;
      type MyFloat is new Float range Float'First .. Float'Last;
 
-----------------------------------------
Using Equality for Floating Point Types
-----------------------------------------

* Questionable due to representation issue

   - As for any programming language
   - Watch out for holes around zero, etc.

* To return True, bits must be exactly the same
* But digital computers approximate real numbers
* Perhaps define your own function

   - Using attributes to determine if values are "close enough"

.. code:: Ada

   Foo : constant Boolean := ( X = 0.01 );
 
--------------------------------
Floating Point Type Attributes
--------------------------------

* Core attributes (for some type named `Real`)

   .. code:: Ada

      type Real is digits N;    -- for some static value N
 
   - `Real'Digits`

      + Yields the number of digits requested (N)

   - `Real'Base'Digits`

      + Yields the number of digits actually provided

   - `Real'Pred` and `Real'Succ`

      + Provide the previous/next implemented number

   - `Real'Rounding( X : Real )`

      + Yields integral value nearest to `X`, rounding away from zero if `X` lies exactly halfway between two integers 

   - Several others

      + See Reference Manual

* Additional model-oriented attributes also exist

   - Provide detailed, more powerful behavior and functionality

===============
Miscellaneous
===============

-----------------------------
 "Checked" Type Conversions
-----------------------------

* Allowed only between "closely related" types

   - Numeric types
   - Types related by inheritance
   - Array types

* Compiler rejects illegal conversions

* Uses explicit functional syntax 

   - As if the target type is the name of a function

.. code:: Ada

   Source_Value : Integer;
   Target_Value : Float;
   ...
   Target_Value := Float (Source_Value);
 
------------------------------
"Unchecked" Type Conversions
------------------------------

* Allowed between any types
* Some practical restrictions may be applied

.. code:: Ada

   function As_Byte is new
      Ada.Unchecked_Conversion (
         Source => Byte_Mask,
         Target => Byte);

.. container:: speakernote

   Sizes should be the same, but not a requirement

---------------------------------
Default Initialization, by Type
---------------------------------

.. admonition:: Language Variant

   Ada 2012

* Not defined by language

   - Access values ("pointers") are the exception

* Not as essential as in some languages

   - Record types can have component default values
   - Since most private types are records, this is often sufficient

* Can be defined for user-defined types
* Applicable for only some kinds of types

   - Scalar types
   - Array types with scalar component types
   - All others are either already covered or not meaningful

      + What would a default task value mean?

-------------------------------
Default Initialization Syntax
-------------------------------

.. admonition:: Language Variant

   Ada 2012

* Uses aspect clauses on type declarations

   .. code:: Ada

      full_type_declaration ::= type defining_identifier is  
         type_definition [aspect_specification];
 
* Only on type declarations, so only on user-defined
* Uses aspect `Default_Value` for scalar types

   - Specified value must be static

   .. code:: Ada

      type Tertiary_Switch is (Off, On, Neither)
         with Default_Value => Neither;
      Implicit : Tertiary_Switch;
      Explicit : Tertiary_Switch := Neither;
 
-------------------------------
Simple Static Type Derivation
-------------------------------

* A way to create a new type from an existing type

   - A limited form of inheritance: not full OOP!

* Syntax specifies new type and existing type names

   - Remember Ada uses named typing

   .. code:: Ada

      derived_type ::= type identifier is new subtype_indication;
      subtype_indication ::= type_or_subtype_name [constraint]
 
* We use it for convenience occasionally

=====
Lab
=====

.. include:: labs/030_basic_types.lab.rst

=========
Summary
=========

--------------------------------------
 Benefits of Strongly Typed Numerics
--------------------------------------

* Designed to prevent subtle bugs
* Cannot accidentally delete a fractional part
* Cannot mix apples and oranges

   .. code:: Ada

      type Yen is range 0 .. 1_000_000;
      type Ruble is range 0 .. 1_000_000;
      Mine : Yen := 1;
      Yours : Ruble := 1;
      Mine := Yours; -- illegal
 
------------------------------------
User-Defined Numeric Type Benefits
------------------------------------

* Reflect application requirements

   - Source code explicitly indicates algorithm requirements (e.g., range or precision)

* Compiler either implements or rejects declaration

   - No surprises or debugging at run-time

* Portability enhanced 

   - Since hardware dependencies reduced

---------
Summary
---------

* User-defined types and strong typing make sense

   - Programs can be written in application's terms
   - Book-keeping can be left to the computer

* No free lunch

   - Security, reliability requirements impose a price
   - Performance roughly the same, given same requirements

* Several classes of type are available for user definitions
* Don't make distinct numeric types for everything

   - Miles, minutes, altitude, velocity, etc...

* Default initialization is possible but use sparingly
