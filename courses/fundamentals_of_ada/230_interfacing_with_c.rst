**********************
Interfacing with C
**********************

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

--------------
Introduction
--------------

* Lots of C code out there already

   - Maybe even a lot of reusable code in your own repositories

* Need a way to interface Ada code with existing C libraries

   - Built-in mechanism to define ability to import objects from C or export Ada objects

* Passing data between languages can cause issues

   - Sizing requirements
   - Passing mechanisms (by reference, by copy)

=================
Import / Export
=================

------------------------------
Pragma Import / Export (1/2)
------------------------------

* :ada:`Pragma Import` allows a C implementation to complete an Ada specification

   - Ada view

      .. code:: Ada

         procedure C_Proc;
         pragma Import (C, C_Proc, "SomeProcedure");

   - C implementation

       .. code:: C

          void SomeProcedure (void) {
             // some code
          }

* :ada:`Pragma Export` allows an Ada implementation to complete a C specification

   - Ada implementation

       .. code:: Ada

          procedure Some_Procedure;
          pragma Export (C, Some_Procedure, "ada_some_procedure");
          procedure Some_Procedure is
          begin
           -- some code
          end Some_Procedure;

   - C view

       .. code:: C

          extern void ada_some_procedure (void);

------------------------------
Pragma Import / Export (2/2)
------------------------------

* You can also import/export variables

   - Variables imported won't be initialized
   - Ada view

      .. code:: Ada

         My_Var : integer_type;
         Pragma Import ( C, My_Var, "my_var" );

   - C implementation

      .. code:: C

         int my_var;

-----------------------------
Import / Export in Ada 2012
-----------------------------

.. admonition:: Language Variant

   Ada 2012

* In Ada 2012, :ada:`Import` and :ada:`Export` can also be done using aspects:

   .. code:: Ada

      procedure C_Proc
        with Import,
             Convention    => C,
             External_Name => "c_proc";

===================
Parameter Passing
===================

-----------------------------
Parameter Passing to/from C
-----------------------------

* The mechanism used to pass formal subprogram parameters and function results depends on:

   - The type of the parameter
   - The mode of the parameter
   - The Convention applied on the Ada side of the subprogram declaration

* The exact meaning of *Convention C*, for example, is documented in *LRM* B.1 - B.3, and in the *GNAT User's Guide* section 3.11.

-----------------------------------
Passing Scalar Data as Parameters
-----------------------------------

* C types are defined by the Standard
* Ada types are implementation-defined
* GNAT standard types are compatible with C types

   - Implementation choice, use carefully

* At the interface level, scalar types must be either constrained with representation clauses, or coming from Interfaces.C
* Ada view

   .. code:: Ada

      with Interfaces.C;
      function C_Proc (I : Interfaces.C.Int)
          return Interfaces.C.Int;
      pragma Import (C, C_Proc, "c_proc");

* C view

   .. code:: C

     int c_proc (int i) {
       /* some code */
     }

-----------------------------------
Passing Structures as Parameters
-----------------------------------

* An Ada record that is mapping on a C struct must:

   - Be marked as convention C to enforce a C-like memory layout
   - Contain only C-compatible types

* C View

   .. code:: C

     enum Enum {E1, E2, E3};
     struct Rec {
        int A, B;
        Enum C;
     };

* Ada View

   .. code:: Ada

     type Enum is (E1, E2, E3);
     Pragma Convention ( C, Enum );
     type Rec is record
       A, B : int;
       C : Enum;
     end record;
     Pragma Convention ( C, Rec );

* Using Ada 2012 aspects

   .. code:: Ada

     type Enum is (E1, E2, E3) with Convention => C;
     type Rec is record
       A, B : int;
       C : Enum;
     end record with Convention => C;

-----------------
Parameter modes
-----------------

* :ada:`in` scalar parameters passed by copy
* :ada:`out` and :ada:`in out` scalars passed using temporary pointer on C side
* By default, composite types passed by reference on all modes except when the type is marked :ada:`C_Pass_By_Copy`

   - Be very careful with records - some C ABI pass small structures by copy!

* Ada View

   .. code:: Ada

      Type R1 is record
         V : int;
      end record
      with Convention => C;

      type R2 is record
         V : int;
      end record
      with Convention => C_Pass_By_Copy;

* C View

   .. code:: C

      struct R1{
         int V;
      };
      struct R2 {
         int V;
      };
      void f1 (R1 p);
      void f2 (R2 p);

====================
Complex Data Types
====================

--------
Unions
--------

* C :C:`union`

   .. code:: C

      union Rec {
         int A;
         float B;
      };

* C unions can be bound using the :ada:`Unchecked_Union` aspect
* These types must have a mutable discriminant for convention purpose, which doesn't exist at run-time

   - All checks based on its value are removed - safety loss
   - It cannot be manually accessed

* Ada implementation of a C :C:`union`

   .. code:: Ada

      type Rec (Flag : Boolean := False) is
      record
         case Flag is
            when True =>
               A : int;
            when False =>
               B : float;
         end case;
      end record
      with Unchecked_Union,
           Convention => C;

--------------------
Arrays Interfacing
--------------------

* In Ada, arrays are of two kinds:

   - Constrained arrays
   - Unconstrained arrays

* Unconstrained arrays are associated with

   - Components
   - Bounds

* In C, an array is just a memory location pointing (hopefully) to a structured memory location

   - C does not have the notion of unconstrained arrays

* Bounds must be managed manually

   - By convention (null at the end of string)
   - By storing them on the side

* Only Ada constrained arrays can be interfaced with C

----------------------
Arrays from Ada to C
----------------------

* An Ada array is a composite data structure containing 2 elements: Bounds and Elements

   - **Fat pointers**

* When arrays can be sent from Ada to C, C will only receive an access to the elements of the array
* Ada View

   .. code:: Ada

      type Arr is array (Integer range <>) of int;
      procedure P (V : Arr; Size : int);
      pragma Import (C, P, "p");

* C View

   .. code:: C

      void p (int * v, int size)  {
      }

----------------------
Arrays from C to Ada
----------------------

* There are no boundaries to C types, the only Ada arrays that can be bound must have static bounds
* Additional information will probably need to be passed
* Ada View

   .. code:: Ada

      -- DO NOT DECLARE OBJECTS OF THIS TYPE
      type Arr is array (0 .. Integer'Last) of int;

      procedure P (V : Arr; Size : int);
      pragma Export (C, P, "p");

      procedure P (V : Arr; Size : int) is
      begin
         for J in 0 .. Size - 1 loop
            -- code;
         end loop;
      end P;

* C View

   .. code:: C

      extern void p (int * v, int size);
      int x [100];
      p (x, 100);

---------
Strings
---------

* Importing a :ada:`String` from C is like importing an array - has to be done through a constrained array
* :ada:`Interfaces.C.Strings` gives a standard way of doing that
* Unfortunately, C strings have to end by a null character
* Exporting an Ada string to C needs a copy!

   .. code:: Ada

      Ada_Str : String := "Hello World";
      C_Str : chars_ptr := New_String (Ada_Str);

* Alternatively, a knowledgeable Ada programmer can manually create Ada strings with correct ending and manage them directly

   .. code:: Ada

      Ada_Str : String := "Hello World" & ASCII.NUL;

* Back to the unsafe world - it really has to be worth it speed-wise!

==============
Interfaces.C
==============

------------------------
Interfaces.C Hierarchy
------------------------

* Ada supplies a subsystem to deal with Ada/C interactions
* :ada:`Interfaces.C` - contains typical C types and constants, plus some simple Ada string to/from C character array conversion routines

   - :ada:`Interfaces.C.Extensions` - some additonal C/C++ types
   - :ada:`Interfaces.C.Pointers` - generic package to simulate C pointers (pointer as an unconstrained array, pointer arithmetic, etc)
   - :ada:`Interfaces.C.Strings` - types / functions to deal with C "char \*"

--------------
Interfaces.C
--------------

.. code:: Ada

   package Interfaces.C is

      --  Declaration's based on C's <limits.h>
      CHAR_BIT  : constant := 8;
      SCHAR_MIN : constant := -128;
      SCHAR_MAX : constant := 127;
      UCHAR_MAX : constant := 255;

      type int   is new Integer;
      type short is new Short_Integer;
      type long  is range -(2 ** (System.Parameters.long_bits - Integer'(1)))
        .. +(2 ** (System.Parameters.long_bits - Integer'(1))) - 1;

      type signed_char is range SCHAR_MIN .. SCHAR_MAX;
      for signed_char'Size use CHAR_BIT;

      type unsigned       is mod 2 ** int'Size;
      type unsigned_short is mod 2 ** short'Size;
      type unsigned_long  is mod 2 ** long'Size;

      type unsigned_char is mod (UCHAR_MAX + 1);
      for unsigned_char'Size use CHAR_BIT;

      type ptrdiff_t is range -(2 ** (System.Parameters.ptr_bits - Integer'(1))) ..
                              +(2 ** (System.Parameters.ptr_bits - Integer'(1)) - 1);

      type size_t is mod 2 ** System.Parameters.ptr_bits;

      --  Floating-Point
      type C_float     is new Float;
      type double      is new Standard.Long_Float;
      type long_double is new Standard.Long_Long_Float;

      type char is new Character;
      nul : constant char := char'First;

      function To_C   (Item : Character) return char;
      function To_Ada (Item : char)      return Character;

      type char_array is array (size_t range <>) of aliased char;
      for char_array'Component_Size use CHAR_BIT;

      function Is_Nul_Terminated (Item : char_array) return Boolean;

      -- (more not specified here)

   end Interfaces.C;

------------------------
Interfaces.C.Extensions
------------------------

.. code:: Ada

   package Interfaces.C.Extensions is

      --  Definitions for C "void" and "void *" types
      subtype void     is System.Address;
      subtype void_ptr is System.Address;

      --  Definitions for C incomplete/unknown structs
      subtype opaque_structure_def is System.Address;
      type opaque_structure_def_ptr is access opaque_structure_def;

      --  Definitions for C++ incomplete/unknown classes
      subtype incomplete_class_def is System.Address;
      type incomplete_class_def_ptr is access incomplete_class_def;

      --  C bool
      type bool is new Boolean;
      pragma Convention (C, bool);

      --  64-bit integer types
      subtype long_long is Long_Long_Integer;
      type unsigned_long_long is mod 2 ** 64;

      -- (more not specified here)

   end Interfaces.C.Extensions;

------------------------
Interfaces.C.Pointers
------------------------

.. code:: Ada

   generic
      type Index is (<>);
      type Element is private;
      type Element_Array is array (Index range <>) of aliased Element;
      Default_Terminator : Element;

   package Interfaces.C.Pointers is

      type Pointer is access all Element;
      for Pointer'Size use System.Parameters.ptr_bits;

      function Value (Ref        : Pointer;
                      Terminator : Element := Default_Terminator)
                      return Element_Array;
      function Value (Ref    : Pointer;
                      Length : ptrdiff_t)
                      return Element_Array;

      Pointer_Error : exception;

      function "+" (Left : Pointer;   Right : ptrdiff_t) return Pointer;
      function "+" (Left : ptrdiff_t; Right : Pointer)   return Pointer;
      function "-" (Left : Pointer;   Right : ptrdiff_t) return Pointer;
      function "-" (Left : Pointer;   Right : Pointer)   return ptrdiff_t;

      procedure Increment (Ref : in out Pointer);
      procedure Decrement (Ref : in out Pointer);

      -- (more not specified here)

   end Interfaces.C.Pointers;

------------------------
Interfaces.C.Strings
------------------------

.. code:: Ada

   package Interfaces.C.Strings is

      type char_array_access is access all char_array;
      for char_array_access'Size use System.Parameters.ptr_bits;

      type chars_ptr is private;

      type chars_ptr_array is array (size_t range <>) of aliased chars_ptr;

      Null_Ptr : constant chars_ptr;

      function To_Chars_Ptr (Item      : char_array_access;
                             Nul_Check : Boolean := False) return chars_ptr;

      function New_Char_Array (Chars : char_array) return chars_ptr;

      function New_String (Str : String) return chars_ptr;

      procedure Free (Item : in out chars_ptr);

      function Value (Item : chars_ptr) return char_array;
      function Value (Item   : chars_ptr;
                      Length : size_t)
                      return char_array;
      function Value (Item : chars_ptr) return String;
      function Value (Item   : chars_ptr;
                      Length : size_t)
                      return String;

      function Strlen (Item : chars_ptr) return size_t;

      -- (more not specified here)

   end Interfaces.C.Strings;

========
Lab
========

.. include:: labs/230_interfacing_with_c.lab.rst

=========
Summary
=========

---------
Summary
---------

* Possible to interface with other languages (typically C)
* Ada provides some built-in support to make interfacing simpler
* Crossing languages can be made safer

   - But it still increases complexity of design / implementation
