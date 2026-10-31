==============================
Address Clauses and Overlays
==============================

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
