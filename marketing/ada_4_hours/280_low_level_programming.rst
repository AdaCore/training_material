***********************
Low Level Programming
***********************

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

=====================
Data Representation
=====================

-------------------------------------
Data Representation vs Requirements
-------------------------------------

* Developer usually defines requirements on a type

   .. code:: Ada

      type My_Int is range 1 .. 10;

* The compiler then generates a representation for this type that can accommodate requirements

   - In GNAT, can be consulted using ``-gnatR2`` switch

      .. code:: Ada

         type Ada2012_Int is range 1 .. 10
            with Object_Size => 8,
                 Value_Size  => 4,
                 Alignment   => 1;

* These values can be explicitly set, the compiler will check their consistency
* They can be queried as attributes if needed

   .. code:: Ada

      X : Integer := My_Int'Alignment;

---------------------
Value_Size / Size
---------------------

* `Value_Size` (or `Size` in the Ada Reference Manual) is the minimal number of bits required to represent data

   - For example, :ada:`Boolean'Size = 1`

* The compiler is allowed to use larger size to represent an actual object, but will check that the minimal size is enough

   .. code:: Ada

      type T1 is range 1 .. 4
         with Size => 3;

-----------
Alignment
-----------

* Number of bytes on which the type has to be aligned
* Some alignment may be more efficient than others in terms of speed (e.g. boundaries of words (4, 8))
* Some alignment may be more efficient than others in terms of memory usage

   .. code:: Ada

      type T1 is range 1 .. 4
         with Size      => 4,
              Alignment => 8;

-------------
Pack Aspect
-------------

* `pack` aspect applies to composite types (record and array)
* Compiler optimizes data for size no matter performance impact
* Unpacked

   .. code:: Ada

      type Enum is (E1, E2, E3);
      type Rec is record
         A : Integer;
         B : Boolean;
         C : Enum;
      end record;
      type Ar is array (1 .. 1000) of Boolean;
      -- Rec'Size is 48, Ar'Size is 8000

* Packed

   .. code:: Ada

      type Enum is (E1, E2, E3);
      type Rec is record
         A : Integer;
         B : Boolean;
         C : Enum;
      end record with Pack;
      type Ar is array (1 .. 1000) of Boolean;
      pragma Pack (Ar);
      -- Rec'Size is 35, Ar'Size is 1000

-------------------------------
Record Representation Clauses
-------------------------------

.. container:: columns

 .. container:: column

    * Exact mapping between a record and its binary representation
    * Optimization purposes, or hardware requirements

       - Driver mapped on the address space, communication protocol...

    * Components represented as

      .. code:: Ada

        <name> at <byte> range
           <starting-bit> ..
           <ending-bit>

 .. container:: column

      .. code:: Ada

        type Rec1 is record
           A : Integer range 0 .. 4;
           B : Boolean;
           C : Integer;
           D : Enum;
        end record;
        for Rec1 use record
           A at 0 range 0 ..  2;
           B at 0 range 3 ..  3;
           C at 0 range 4 .. 35;
           -- unused space here
           D at 5 range 0 ..  2;
        end record;

------------------------------
Array Representation Clauses
------------------------------

* :ada:`Component_Size` for array's **component's** size

.. code:: Ada

   type Ar2 is array (1 .. 1000) of Boolean
       with Component_Size => 2;

==============================
Address Clauses and Overlays
==============================

-----------------
Address Clauses
-----------------

* Ada allows specifying the address of an entity

   .. code:: Ada

      Var : Unsigned_32 with Address => 16#1234_ABCD#;

* Very useful to declare I/O registers

   - For that purpose, the object should be declared volatile:

   .. code:: Ada

      Var : Unsigned_32 with Volatile;

* Useful to read a value anywhere

   .. code:: Ada

      function Get_Byte (Addr : Address) return Unsigned_8 is
        V : Unsigned_8 with Volatile, Address => Addr;
      begin
        return V;
      end;

   - In particular the address doesn't need to be constant
   - But must match alignment

----------------------
Unchecked Conversion
----------------------

* `Unchecked_Conversion` allows an unchecked *bitwise* conversion of data between two types
* Needs to be explicitly instantiated

   .. code:: Ada

      type Bitfield is array (1 .. Integer'Size) of Boolean;
      function To_Bitfield is new
         Ada.Unchecked_Conversion (Integer, Bitfield);
      V : Integer;
      V2 : Bitfield := To_Bitfield (V);

* Avoid conversion if the sizes don't match

   - Not defined by the standard
   - Many compilers will warn if the type sizes do not match

=================
Inline Assembly
=================

------------------
Simple Statement
------------------

* Instruction without inputs/outputs

   .. code:: Ada

      Asm ("halt", Volatile => True);

   - You may specify `Volatile` to avoid compiler optimizations
   - In general, keep it False unless it created issues

* You can group several instructions

   .. code:: Ada

      Asm ("nop" & ASCII.LF & ASCII.HT
           & "nop", Volatile => True);
      Asm ("nop; nop", Volatile => True);

* The compiler doesn't check the assembly, only the assembler will

   - Error message might be difficult to read

-----------------------------------
Instruction Counter Example (x86)
-----------------------------------

.. code:: Ada

   with System.Machine_Code; use System.Machine_Code;
   with Ada.Text_IO;         use Ada.Text_IO;
   with Interfaces;          use Interfaces;
   procedure Main is
      Low   : Unsigned_32;
      High  : Unsigned_32;
      Value : Unsigned_64;
      use ASCII;
   begin
      Asm ("rdtsc" & LF,
           Outputs =>
              (Unsigned_32'Asm_Output ("=g", Low),
               Unsigned_32'Asm_Output ("=a", High)),
           Volatile => True);
      Values := Unsigned_64 (Low) +
                Unsigned_64 (High) * 2 ** 32;
      Put_Line (Values'Image);
   end Main;
