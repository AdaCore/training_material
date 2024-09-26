==============
Introduction
==============

--------------
Introduction
--------------

* :dfn:`Overloading` is the use of an already existing name to define a **new** entity
* Historically, only done as part of the language **implementation**

   - Eg. on operators
   - Float vs Integer vs pointers arithmetic

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
Overloadable Entities in Ada
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

      function "+" (L, R : Integer) return String is
      begin
         return Integer'Image (L - R);
      end "+";

