==============================
Address Clauses and Overlays
==============================

---------
Address
---------

* Ada distinguishes the notions of

   - A reference to an object
   - An abstract notion of address (:ada:`System.Address`)
   - The integer representation of an address

* Safety is preserved by letting the developer manipulate the right level of abstraction
* Conversion between pointers, integers and addresses are possible
* The address of an object can be specified through the :ada:`Address` aspect

-----------------
Address Clauses
-----------------

* Ada allows specifying the address of an entity

   .. code:: Ada

      Use_Aspect     : Unsigned_32 with
         Address => 16#1234_ABCD#;

      Use_Rep_Clause : Unsigned_32;
      for Use_Rep_Clause'Address use 16#5678_1234#;

* Very useful to declare I/O registers

   - For that purpose, the object should be declared volatile:

   .. code:: Ada

      Use_Aspect     : Unsigned_32 with
         Volatile,
         Address => 16#1234_ABCD#;

      Use_Rep_And_Pragma : Unsigned_32;
      for Use_Rep_And_Pragma'Address use 16#5678_1234#;
      pragma Volatile (Use_Rep_And_Pragma);

* Useful to read a value anywhere

   .. code:: Ada

      function Get_Byte (Addr : Address) return Unsigned_8 is
        V : Unsigned_8 with Address => Addr, Volatile;
      begin
        return V;
      end;

   - In particular the address doesn't need to be constant
   - But must match alignment

----------------
Address Values
----------------

* The type :ada:`Address` is declared in :ada:`System`

   - But this is a :ada:`private` type
   - You cannot use a number

* Ada standard way to set constant addresses:

   - Use :ada:`System.Storage_Elements` which allows arithmetic on address

   .. code:: Ada

      V : Unsigned_32 with
          Address =>
              System.Storage_Elements.To_Address (16#120#);

* GNAT specific attribute :ada:`'To_Address`

   - Handy but not portable

   .. code:: Ada

      V : Unsigned_32 with
          Address => System'To_Address (16#120#);

----------
Volatile
----------

* The :ada:`Volatile` property can be set using an aspect or a pragma
* Ada also allows volatile types as well as objects

   .. code:: Ada

      type Volatile_U32 is mod 2**32 with Volatile;
      type Volatile_U16 is mod 2**16;
      pragma Volatile (Volatile_U16);

* The exact sequence of reads and writes from the source code must appear in the generated code

   - No optimization of reads and writes

* Volatile types are passed by-reference

---------------------
Ada Address Example
---------------------

.. code:: Ada

   type Bit_Array_T is array (Integer range <>) of Boolean
      with Component_Size => 1;

   -- objects can be referenced elsewhere
   Object  : aliased Integer with Volatile;
   Object2 : aliased Integer with Volatile;

   Object_A : System.Address := Object'Address;
   Object_I : Integer_Address := To_Integer (Object_A);

   --  This overlays Bit_Array_Object onto Object in memory
   Bit_Array_Object : aliased Bit_Array_T (1 .. Object'Size)
      with Address => Object_A;

   Object2_Alias : aliased Integer
      --  Trust me, I know what I'm doing, this is Object2
      with Address => To_Address (Object_I - 4);

--------------------
Aliasing Detection
--------------------

* :dfn:`Aliasing`: multiple objects are accessing the same address

   - Types can be different
   - Two pointers pointing to the same address
   - Two references onto the same address
   - Two objects at the same address

* :ada:`Var1'Has_Same_Storage (Var2)` checks if two objects occupy exactly the same space
* :ada:`Var'Overlaps_Storage (Var2)` checks if two object are partially or fully overlapping

----------------------
Unchecked Conversion
----------------------

* :ada:`Unchecked_Conversion` allows an unchecked *bitwise* conversion of data between two types
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

