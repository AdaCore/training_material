==============
Interfaces.C
==============

------------------------
Interfaces.C Hierarchy
------------------------

* Ada supplies a subsystem to deal with Ada/C interactions
* :ada:`Interfaces.C` - contains typical C types and constants, plus some simple Ada string to/from C character array conversion routines

   - :ada:`Interfaces.C.Extensions` - some additional C/C++ types
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
      type Component is private;
      type Component_Array is array (Index range <>) of aliased Component;
      Default_Terminator : Component;

   package Interfaces.C.Pointers is

      type Pointer is access all Component;
      for Pointer'Size use System.Parameters.ptr_bits;

      function Value (Ref        : Pointer;
                      Terminator : Component := Default_Terminator)
                      return Component_Array;
      function Value (Ref    : Pointer;
                      Length : ptrdiff_t)
                      return Component_Array;

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

