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
Arrays From Ada to C
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
Arrays From C to Ada
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

