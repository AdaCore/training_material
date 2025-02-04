=====================
Data Representation
=====================

-------------------------------------
Data Representation Vs Requirements
-------------------------------------

* Developer usually defines requirements on a type

   .. code:: Ada

      type My_Int is range 1 .. 10;

* The compiler then generates a representation for this type that can accommodate requirements

   - In GNAT, can be consulted using ``-gnatR2`` switch

.. code:: Ada

   -- with aspects
   type Some_Integer_T is range 1 .. 10
      with Object_Size => 8,
           Value_Size  => 4,
           Alignment   => 1;

   -- with representation clauses
   type Another_Integer_T is range 1 .. 10;
   for Another_Integer_T'Object_Size use 8;
   for Another_Integer_T'Value_Size  use 4;
   for Another_Integer_T'Alignment   use 1;

* These values can be explicitly set, the compiler will check their consistency
* They can be queried as attributes if needed

   .. code:: Ada

      X : Integer := My_Int'Alignment;

---------------------
Value_Size / Size
---------------------

* :ada:`Value_Size` (or :ada:`Size` in the Ada Reference Manual) is the minimal number of bits required to represent data

   - For example, :ada:`Boolean'Size = 1`

* The compiler is allowed to use larger size to represent an actual object, but will check that the minimal size is enough

.. code:: Ada

   -- with aspect
   type Small_T is range 1 .. 4
      with Size => 3;

   -- with representation clause
   type Another_Small_T is range 1 .. 4;
   for Another_Small_T'Size use 3;

-----------------------------
Object Size (GNAT-Specific)
-----------------------------

* :ada:`Object_Size` represents the size of the object in memory
* It must be a multiple of :ada:`Alignment * Storage_Unit (8)`, and at least equal to :ada:`Size`

.. code:: Ada

   -- with aspects
   type Some_T is range 1 .. 4
      with Value_Size  => 3,
           Object_Size => 8;

   -- with representation clauses
   type Another_T is range 1 .. 4;
   for Another_T'Value_Size use 3;
   for Another_T'Object_Size use 8;

* Object size is the *default* size of an object, can be changed if specific representations are given

-----------
Alignment
-----------

* Number of bytes on which the type has to be aligned
* Some alignment may be more efficient than others in terms of speed (e.g. boundaries of words (4, 8))
* Some alignment may be more efficient than others in terms of memory usage

.. code:: Ada

   -- with aspects
   type Aligned_T is range 1 .. 4
      with Size      => 4,
           Alignment => 8;

   -- with representation clauses
   type Another_Aligned_T is range 1 .. 4;
   for Another_Aligned_T'Size use 4;
   for Another_Aligned_T'Alignment use 8;

--------------
Record Types
--------------

.. container:: columns

 .. container:: column

    * Ada doesn't force any particular memory layout
    * Depending on optimization of constraints, layout can be optimized for speed, size, or not optimized

    .. code:: Ada

       type Enum is (E1, E2, E3);
       type Rec is record
          A : Integer;
          B : Boolean;
          C : Boolean;
          D : Enum;
       end record;

 .. container:: column

    .. image:: record_packing_examples.png
       :width: 50%

-------------
Pack Aspect
-------------

* :ada:`Pack` aspect (or pragma) applies to composite types (record and array)
* Compiler optimizes data for size no matter performance impact
* Unpacked

   .. code:: Ada

      type Enum is (E1, E2, E3);
      type Rec is record
         A : Integer;
         B : Boolean;
         C : Boolean;
         D : Enum;
      end record;
      type Ar is array (1 .. 1000) of Boolean;
      -- Rec'Size is 56, Ar'Size is 8000

* Packed

   .. code:: Ada

      type Enum is (E1, E2, E3);
      type Rec is record
         A : Integer;
         B : Boolean;
         C : Boolean;
         D : Enum;
      end record with Pack;
      type Ar is array (1 .. 1000) of Boolean;
      pragma Pack (Ar);
      -- Rec'Size is 36, Ar'Size is 1000

-------------------------------
Enum Representation Clauses
-------------------------------

* Can specify representation for each value
* Representation must have increasing number

.. code:: Ada

   type E is (A, B, C);
   for E use (A => 2, B => 4, C => 8);

* Can use :ada:`E'Enum_Rep (A) = 2`
* Can use :ada:`E'Enum_Val (2) = A`

-------------------------------
Record Representation Clauses
-------------------------------

.. container:: columns

 .. container:: column

    * Exact mapping between a record and its binary representation
    * Optimization purposes, or hardware requirements

       - Driver mapped on the address space, communication protocol...

    * Fields represented as

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

------------------
Unchecked Unions
------------------

* Allows replicating C's :c:`union` with **discriminated** records
* Discriminant is **not stored**
* No discriminant check
* Object must be **mutable**

.. code:: Ada

    type R (Is_Float : Boolean := False) is record
        case Is_Float is
        when True =>
            F : Float;
        when False =>
            I : Integer;
        end case;
    end record
        with Unchecked_Union;

    O : R := (Is_Float => False, I => 1);
    F : Float := R.F; --  no check!

------------------------------
Array Representation Clauses
------------------------------

* :ada:`Component_Size` for array's **component's** size

.. code:: Ada

   -- with aspect
   type Array_T is array (1 .. 1000) of Boolean
       with Component_Size => 2;

   -- with representation clause
   type Another_Array_T is array (1 .. 1000) of Boolean;
   for Another_Array_T'Component_Size use 2;

--------------------------
Endianness Specification
--------------------------

* :ada:`Bit_Order` for a type's endianness
* :ada:`Scalar_Storage_Order` for composite types

    - Endianess of components' ordering
    - GNAT-specific
    - Must be consistent with :ada:`Bit_Order`

* Compiler will peform needed bitwise transformations when performing operations

.. code:: Ada

   -- with aspect
   type Array_T is array (1 .. 1000) of Boolean with
     Scalar_Storage_Order => System.Low_Order_First;

   -- with representation clauses
   type Record_T is record
      A : Integer;
      B : Boolean;
   end record;
   for Record_T use record
      A at 0 range 0 .. 31;
      B at 0 range 32 .. 33;
   end record;
   for Record_T'Bit_Order use System.High_Order_First;
   for Record_T'Scalar_Storage_Order use System.High_Order_First;

--------------------------
Change of Representation
--------------------------

* Explicit new type can be used to set representation
* Very useful to unpack data from file/hardware to speed up references

.. code:: Ada

    type Rec_T is record
         Field1 : Unsigned_8;
         Field2 : Unsigned_16;
         Field3 : Unsigned_8;
    end record;
    type Packed_Rec_T is new Rec_T;
    for Packed_Rec_T use record
       Field1 at 0 range  0 ..  7;
       Field2 at 0 range  8 .. 23;
       Field3 at 0 range 24 .. 31;
    end record;
    R : Rec_T;
    P : Packed_Rec_T;
    ...
    R := Rec_T (P);
    P := Packed_Rec_T (R);

.. container:: speakernote

   Size of R is probably 48 (for 16-bit alignment) or 96 (for 32-bit alignment)
   Size of P will always be 32

