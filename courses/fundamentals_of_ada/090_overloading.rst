
*************
Overloading
*************

==============
Introduction
==============

--------------
Introduction
--------------

* Overloading is the use of an already existing name to define a new entity
* Historically, only done within the language

   - FORTRAN

      + Numeric operators for complex, real, integer, ...

   - Pascal

      + `WRITELN( 42 );`
      + `WRITELN( 'This is a string' );`

   - Many others...

* Most modern languages allow users to do it too

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

   type Complex is record
     Real_Part : Float;
     Imaginary : Float;
   end record;
   ...
   function "+" (L,R : Complex) return Complex is
   begin
     return (L.Real_Part + R.Real_Part,
             L.Imaginary + R.Imaginary);
   end "+";
   ...
   A, B, C : Complex;
   I, J, K : Integer;
   ...
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

      function "+" ( L, R : integer ) return string is
      begin
         return integer'image ( L - R );
      end "+";
 
=========================
Enumerals and Operators
=========================

----------
Examples
----------

.. include:: examples/090_overloading/enumerals_and_operators.rst

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

   - Number of parameters must remain same

      + Unary, Binary, etc ...

   - No default expressions allowed for operators

      + Since number of parameters must remain the same

* Infix calls use positional parameter associations

   - Left actual goes to first formal, right actual goes to second formal
   - Definition

      .. code:: Ada

         function "*" (Left, Right : Integer) return integer;
 
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

-----------------
Call Resolution
-----------------

* Compilers must reject ambiguous calls
* Resolution is based on the calling context

   - Compiler attempts to find a matching specification

      + Based on *Parameter and Result Type Profile*

   - More than one matching specification is ambiguous

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

   - Number of parameters
   - Order of parameters
   - Base type of parameters
   - Result type (for functions)

* Insignificant components might not appear at call

   - Formal parameter names are optional
   - Formal parameter modes never appear
   - Formal parameter subtypes never appear
   - Default expressions never appear

   .. code:: Ada

      Display (X);
      Display (Foo => X);
      Display (Foo => X, Bar => Y);
 
-------------------------------
Manually Disambiguating Calls
-------------------------------

* Qualification can be used
* Named parameter association can be used

   - If unique

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
    
   function "+" (Left : Position;
                 Right : Offset)
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

===================
Visibility Issues
===================

----------
Examples
----------

.. include:: examples/090_overloading/visibility_issues.rst

-----------------------------------
Inherently Ambiguous Declarations
-----------------------------------

* When a profile appears again within a single scope
* Are illegal since all calls would be ambiguous

   .. code:: Ada

      procedure Test is
        procedure P (X : in Natural) is ...
        procedure P (A : in out Positive) is ... 
      begin
        ...
      end Test;
 
* Compile error

   .. code:: Ada

      test.adb:3:04: duplicate body for "P" declared at line 2
 
----------------
Profile Hiding
----------------

* Subprograms can hide others with same profile
* Only when scopes differ (same scope would imply illegal declarations)

.. code:: Ada

   Outer : declare
     procedure P (X : in Natural := 0) is ...
     begin
       declare
         procedure P (A : in out Positive) is ...
       begin
         P ( ... );  -- not Outer.P
       end;
   end Outer;
 
=======================
User-Defined Equality
=======================

----------
Examples
----------

.. include:: examples/090_overloading/user_defined_equality.rst

-----------------------
User-Defined Equality
-----------------------

* Allowed like any other operator

   - Must remain a binary operator

* May have any parameter and result types

   - Typically declared to return type Boolean

* Non-Boolean result example:

   .. code:: Ada

      type Fuzzy_Result is (Unknown, False, True);
      function "=" (Left : Foo;  Right : Bar)
          return Fuzzy_Result;

------------------------------------
User-Defined `=` Returning Boolean
------------------------------------

* Implicitly declares ``/=``
* Thus negation has consistent meaning

   .. code:: Ada

      if X /= Y then
      if not ( X = Y ) then
      if X not = Y then
 
* No explicit declaration of ``/=`` returning Boolean 

   - Returning values of other types is allowed

      .. code:: Ada

         function "/=" (Left : Foo;  Right : Bar)
             return Fuzzy_Result;
 
-------------------------------
User-Defined Equality Example
-------------------------------

* Especially useful for composite types
* Predefined ``=`` is bit-wise comparison over entire structure so may be inappropriate semantics
* Given the following types:
    
   .. code:: Ada
    
      Max : constant := 100;
      type Index is range 0 .. Max;
      type List is array (Index range 1 .. Max) of Integer;
      type Stack is record
        Values : List;
        Top : Index := 0;
      end record;
     
* Equality function might look like:

   .. code:: Ada
    
      function "=" (Left, Right : Stack) return Boolean is
      begin
        if Left.Top /= Right.Top then -- not same size
          return False;
        else -- compare values
          for K in 1 .. Left.Top loop
            if Left.Values(K) /= Right.Values(K) then
              return False;
            end if;
          end loop;
        end if;
        return True;
      end "=";
     
=========================
Composition of Equality
=========================

----------
Examples
----------

.. include:: examples/090_overloading/composition_of_equality.rst

----------------------------
 "Composition of Equality"
----------------------------

* Whether user-defined equality functions are called automatically as part of equality for composite types containing types having such functions
* Only composes when user-defined equality is defined

   * Assume you defined "=" for a scalar type
   * If you define "=" for a composite containing the scalar type, your scalar "=" will be used
   * If you rely on the implicit "=" for the composite, then the scalar's implicit "=" will also be used

      * Not the one you just defined

--------------------------------
Composition vs Non-Composition
--------------------------------

.. code:: Ada
    
   with Ada.Text_IO; use Ada.Text_IO;
   procedure Main is

      type Array1_T is array (1 .. 3) of Integer;
      type Array2_T is array (1 .. 3) of Integer;

      X, Y     : Integer  := 123;
      X_A, Y_A : Array1_T := (others => 123);
      X_B, Y_B : Array2_T := (others => 123);

      -- When comparing integers directly, this function forces those comparisons
      -- to be false
      function "=" (L, R : Integer) return Boolean is (False);
      -- We define our own array equality operator so it will use our integer operator
      function "=" (L, R : Array2_T) return Boolean is (for all I in 1 .. 3 => L (I) = R (I));

   begin
      -- Use local "=" for integer comparison
      Put_Line (Boolean'Image (X = Y));
      Put_Line (Boolean'Image (X_A (2) = Y_A (2)));
      -- This array comparison uses the predefined operator, so our local "=" is ignored
      Put_Line (Boolean'Image (X_A = Y_A));
      -- This array comparison uses our operator, so our local "=" is used as well
      Put_Line (Boolean'Image (X_B = Y_B));
   end Main;
     
.. container:: speakernote

   Equality for IntegerList doesn't compose because Integer is not a record type.

-------------------------------------
Enclosing Equality Function Example
-------------------------------------

* Explicitly declared for the enclosing type

* Calls user-defined ``=`` for components 

.. code:: Ada

   type Bar is record
     Value : Foo; -- assuming Foo is not a record type
     Id : Integer;
   end record;
   
   function "=" (Left, Right : Bar) return Boolean is
   begin
     -- User-defined "=" for Foo
     return Left.Value = Right.Value 
        -- predefined "=" for integer
        and Left.Id = Right.Id;
   end "=";
 
----------------------------------------
`=` for Predefined Composites Composes
----------------------------------------

* Per RM 4.5.2(32/1)
* For all non-limited types declared in language-defined packages
* Thus you can safely ignore the issue for composite types defined by the language

-----------------------------------
User-Defined Equality Composition
-----------------------------------

* No issue for all language-defined types in all versions of Ada

* An issue for user-defined types 

* Only automatic for `record` types in Ada 2012
* Only automatic for `tagged record` types in Ada 2005

   - Otherwise need explicit equality function for enclosing type

* Not automatic for other user-defined types in any Ada version

   - Need explicit equality function for enclosing type

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

   - Remember `=` for record types does compose, otherwise not
