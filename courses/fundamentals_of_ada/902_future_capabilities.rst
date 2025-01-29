***********************
Future Ada Capabilities
***********************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. role:: Rust(code)
    :language: Rust

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

============
Introduction
============

------------
Introduction
------------

* AdaCore is developing post-Ada 2022 capabilities

   - For now, treated as extensions to the language

* Topics include

   - Embedded programming
   - Targeted quality of life improvements
   - New / enhanced programming paradigms

* Some are implemented, some are under discussions

   - Already implemented features have no marking
   - Features currently being implemented are marked *In Progress* under the slide title
   - Features under discussion are marked *Under Discussion* under the slide title

------------------------
How to Enable Extensions
------------------------

* Two experimental modes now

   - :command:`-gnatx` Enable curated set of experimental features
   - :command:`-gnatx0` Enable all experimental features

* Not recommended for safety critical code

* Useful to share feedback and input

====================
Embedded Programming
====================

---------------------------------------
Fixed Lower Bound for Array Types (1/3)
---------------------------------------

* Ada arrays carry 3 pieces of information

   - Lower bound
   - Upper bound
   - Actual data

* Dealing with lower / upper bound instead of size has consequences

   - Usage is more flexible
   - Access to data requires more arithmetic
   - Potential errors, e.g. when addressing slices

   .. code:: Ada

      function P (S : String) return Character is
      begin
         return S (1); -- May be out of bounds
      end P;

---------------------------------------
Fixed Lower Bound for Array Types (2/3)
---------------------------------------

* Ada arrays can have fixed lower bound (FLB)

   .. code:: Ada

      type Arr is array (Integer range 0 .. <>) of Integer;

* Lower bound must be explicit at instantiation

   .. code:: Ada

      V1 : Arr (0 .. 9); -- OK
      V2 : Arr (1 .. 10); -- Constraint Error

* Objects are automatically adjusted

   .. code:: Ada

      function P (A : Arr) return Character;

      P (V (6 .. 10)); -- A is 0 .. 4

---------------------------------------
Fixed Lower Bound for Array Types (3/3)
---------------------------------------

* FLB can be used for matrixes

   .. code:: Ada

      type Matrix is array (Natural range 0 .. <>,
                            Natural range 0 .. <>) of Integer;

* FLB can be used for subtypes

   .. code:: Ada

      subtype String_1 is String (1 .. <>);

* *Recommendation:* unless specifically needed, use FLB

--------------------
Storage Models (1/2)
--------------------

* Ada offers :ada:`Storage_Pools` to override allocation/deallocation for access types

* Not usable in small runtime context

  * Requires finalization

* Doesn't support most advanced usage with segregated memory regions

  * e.g. CUDA

--------------------
Storage Models (2/2)
--------------------

* Storage model works without :ada:`tagged` types

  * Allows definition of copy primitives

.. container:: latex_environment tiny

  .. container:: columns

    .. container:: column

      .. code:: Ada

         type CUDA_Storage_Model is null record
            with Storage_Model_Type => (
               Allocate     => CUDA_Allocate,
               Deallocate   => CUDA_Deallocate,
               Copy_To      => CUDA_Copy_To,
               Copy_From    => CUDA_Copy_From);

          CUDA_Memory : CUDA_Storage_Model;

    .. container:: column

      .. code:: Ada

        type Host_Array_Access is access all Integer_Array;
        type Device_Array_Access is access Integer_Array
           with Designated_Storage_Model => CUDA_Memory;

        Host_Array : Host_Array_Access
                   := new Integer_Array (1 .. 10);
        Device_Array : Device_Array_Access
                   := new Host_Array (1 .. 10);

     .. code:: Ada

        Host_Array.all := (others => 0);
        --  CUDA_Storage_Model.Copy_To will perform copy
        Device_Array.all := Host_Array.all;

        -- ...

        --  CUDA_Storage_Model.Copy_From will perform copy
        Host_Array.all := Device_Array.all;

--------------------------------------------
Constant Size for Variable Objects (1/3)
--------------------------------------------

.. admonition:: Language Variant

   In progress

* Ada already allows some variable types to have a fixed size

   .. code:: Ada

      type Rec (V : Boolean := True) is record
         case V is
            when True =>
               X : Integer;
             when False =>
               Y, Z : Integer;
         end case;
       end record;

       V : Rec;

       V := (True, 1);
       V := (False, 2, 3);

* We're extending this mechanism to arrays and tagged types

--------------------------------------------
Constant Size for Variable Objects (2/3)
--------------------------------------------

.. admonition:: Language Variant

   In progress

* `'Size'Class` will allow specifing fixed size for a whole hierarchy

.. code:: Ada

   type Foo is tagged abstract null record
      with Size'Class => 16 * 8; -- Size is in bits

   type Bar is new Foo with record
      S : String (1 .. 128);
   end record; -- ERROR: Record doesn't fit in 16 bytes

   type Baz is new Foo with record
      A, B : Integer;
   end record; -- Valid

   -- Valid use cases
   Inst : Foo'Class;
   Inst := Foo'(null record);
   Inst := Baz'(12, 15);

   type Foo_Array is array (Positive range <>) of Foo'Class;
   Arr : Foo_Array := (Foo'(null record), Baz'(12, 15));

--------------------------------------------
Constant Size for Variable Objects (3/3)
--------------------------------------------

.. admonition:: Language Variant

   In progress

* `Definite` will instruct an array instance to always have the maximum size

* Used size of the array can varry over time.

* `'Capacity` will return maximum number of elements of an array

.. code:: Ada

   declare
      type Index is 1 .. 8;
      type Static_Array is array (Index range <>)
          of Natural with Definite;

      A : Static_Array := (1, 2, 3, 4);
      B : Static_Array := (2, 3);
   begin
      A := B;
      A := A & S;
      A := A & (8, 9);
      pragma Assert (A'Capacity = 8);
      -- forbidden as it exceeds the array capacity
      A := (0, 1, 2, 3, 4, 5, 6, 7, 8);
   end;

-------------------------------
Embed Data From Binary File
-------------------------------

.. admonition:: Language Variant

   In progress

* Statically embed content at compile time, as part of the binary
* Avoid either painful encoding of data as aggregates, or impractical (or even impossible in embedded contexts) loading of assets at run-time

.. code:: Ada

   package body Some_Package is
      type Byte is mod 256;
      type Byte_Array is (Integer range <>) of Byte;
      Some_File_Data : Byte_Array
        with External_Initialization => "/some/file/data.raw";

      type Some_Record is record
          X : Integer;
          Y : Char;
      end record;

      Some_Other_Data : Some_Record
        with External_Initialization => "/some/data.raw";
   end Some_Package;

--------------------------
Access to Array Slices
--------------------------

.. admonition:: Language Variant

   In progress

* As of today, it is not possible to create a pointer to a slice of array

* This can be very useful, e.g. when implementing processing on byte arrays

.. code:: Ada

   type X is array (Integer range <>) of Byte;
   type A is access all X:

   V : A := new X (1 .. 100);
   V2 : A := V.all (20 .. 30)'Access

* This requires GNAT redesign of so-called fat pointers

-------------------------------------
Creation of Ada Array From Memory
-------------------------------------

.. admonition:: Language Variant

   Under discussion

* Importing pre-allocated arrays (e.g. from C) is very hard

* There's no way to "build the bounds" manually

.. code:: Ada

   type X is array (Integer range <>) of Integer;
   type A is access all X:

   S : System.Address :=
      <stuff coming from either C or some memory buffer>.

   V : A := X'From_Address (S, 0, 10);
   -- Create a fat pointer of 11 elements
   --   first = 0, last = 10, pointed to by S.

* This also requires GNAT redesign of so-called fat pointers

===============
Quality of Life
===============

------------------------------------------------
Dot Calls for Primitives of Untagged Types (1/2)
------------------------------------------------

* Ada 2012 introduced prefix notation

   .. code:: Ada

      type My_Record is tagged null record;
      type Op1 (V : My_Record);
      type Op2 (V : My_Record);

      ...

      X : My_Record;

      ...

      X.Op1;
      X.Op2;

* Prefix notation is only available for tagged types

* Some users introduce tagged types JUST for the purpose of using prefixed notation

------------------------------------------------
Dot Calls for Primitives of Untagged Types (2/2)
------------------------------------------------

* All primitives can now be accessed through prefix notation

* Requires the first parameter to be the type of the primitives

   .. code:: Ada

      type R is null record;
      procedure Op1 (V : R);

      type I is new Integer;
      procedure Op2 (V : I);

      ...

      VR : R;
      VI : I;

      ...

      VR.Op1;
      VI.Op2;

------------------------------------
Default for Generic Formal Functions
------------------------------------

* Generic can already be given a default matching function

   .. code:: Ada

      generic
         type T is private;
         with function Copy (Item : T) return T
            is (<>); -- Defaults to Copy if any
      package Stacks is

* Generic can now be given a default expression function

   .. code:: Ada

      generic
         type T is private;
         with function Copy (Item : T) return T
           is (Item); -- Defaults to the Item value
      package Stacks is

--------------------
String Interpolation
--------------------

* Constructing strings with expressions can be cumbersome

   .. code:: Ada

      procedure Test_Interpolation is
         X : Integer := 12;
         Y : Integer := 15;
         Name : String := "Leo";
      begin
         Put_Line ("Name is " & Name &
                   " and Sum is " &
                   Integer'Image (X + Y) & ".");
      end;

* Simlar to other languages, Strings can now be interpolated

   .. code:: Ada

       Put_Line (f"Name is {Name} and Sum is {X + Y}.");

--------------------------------------
Declare Local Variables Without Blocks
--------------------------------------

* Restrictions on declarative parts makes less sense today

* Variables can be scoped and declared without introduction of blocks

.. code:: Ada

   if X > 5 then
      X := X + 1;
      Squared : constant Integer := X**2;
      X := X + Squared;
   else
      X := X - 1;
      Cubed : constant Integer := X**3;
      X := X
   end if;

-----------------------------
Conditional "when" Constructs
-----------------------------

* Ada already support :ada:`exit when` structure shortening if-condititions

   .. code:: Ada

      loop
         I := I + 1;
         exit when I > 20;
      end loop;

* when is now expanded to a number of new constructs

   .. code:: Ada

      return when Condition;
      return True when I = 0;
      goto Cleanup when Flags (1);
      raise Error when Imported_C_Func /= 0;
      raise Error with "Unix Error"
         when Imported_C_Func /= 0;

-------------------------
Deep Delta Aggregates
-------------------------

.. admonition:: Language Variant

   In progress

* Delta aggregate can be very verbose when describing sub components

   .. code:: Ada

      (X with delta A => (X.A with delta B => 42))

* Deep delta aggregates will allow to refer to subcomponents directly:

   .. code:: Ada

      (X with delta A.B => 42)

-------------------------------------------
Guaranteed Final Control Flow Execution
-------------------------------------------

.. admonition:: Language Variant

   Under discussion

* Ada supports finalization through controlled types

* Finalization points could be instrumented like other languages

  .. code:: Ada

   function X return Integer is
      V : Integer_Access := new Integer;
   begin
      return V.all;
   exception
      when others => return 0;
   finally
      Free (V);
   end X;


====================================
New / Enhanced Programming Paradigms
====================================

---------------------------
Case Pattern Matching (1/3)
---------------------------

.. admonition:: Language Variant

   In discussion

* Inspired by ML-style languages (Haskell, OCaml)

* Present in most languages nowadays

   .. code:: Ada

      type Rec is record
         F1, F2 : Integer;
      end record;

      procedure Caser_1 (X : Rec) is
      begin
         case X is
            when (F1 => Positive, F2 => Positive) =>
               Do_This;
            when (F1 => Natural, F2 => <>) | (F1 => <>, F2 => Natural) =>
               Do_That;
            when others =>
               Do_The_Other_Thing;
         end case;
      end Caser_1;

---------------------------
Case Pattern Matching (2/3)
---------------------------

.. admonition:: Language Variant

   In discussion

.. code:: Ada

   type Shape is tagged record
      X, Y : Integer;
   end record;

   type Line is new Shape with record
      X2, Y2 : Integer;
   end record;

   type Circle is new Shape with record
      Radius : Natural;
   end record;

   S : Shape'Class := ...;

   case S is
      when Circle'Class'(Radius => 0, others => <>) => Put_Line ("point");
      when Circle'Class => Put_Line ("circle");
      when Line'Class => Put_Line ("line");
      when <> => Put_Line ("other shape");
   end case;

---------------------------
Case Pattern Matching (3/3)
---------------------------

.. admonition:: Language Variant

   In discussion

Pattern matching allows to bind specific values and use them

.. code:: Ada

   type Opt (Has_Value : Boolean) is record
      case Has_Value is
         when True =>
            Val : Int;
         when others => null;
      end case;
   end record;

   case I is
      when (Has_Value => True, Val => <> as V : Integer) =>
         return V;
      when (Has_Value => False) => 0;
   end case;

-----------------------------------------
Generic Instantiation Inference (1/2)
-----------------------------------------

.. admonition:: Language Variant

   Under discussion

* Generic instantiation in Ada is heavy (but safe)

.. code:: Ada

   generic
      type Index_Type is (<>);
      type El_Type is private;
      type Array_Type is array (Index_Type range <>)
         of El_Type;
   function Reduce (Init : Accum; Arr : Array_Type)
            return Accum;

   function My_Reduce is new Reduce (
    Integer,
    Float,
    Float_Array);

   V : Float := My_Reduce (Some_Array);

-----------------------------------------
Generic Instantiation Inference (2/2)
-----------------------------------------

.. admonition:: Language Variant

   Under discussion

* We could implicitly instantiate stateless generics

  .. code:: Ada

   V : Float :=
       Reduce (Integer, Float, Float_Array) (Some_Array);

* We could implicitly infer generic parameters

  .. code:: Ada

    function My_Reduce is new Reduce (
      Array_Type => Float_Array);

* The two above could be combined

  .. code:: Ada

    V : Float :=
        Reduce (Array_Type => Float_Array) (Some_Array);

----------------------------------------
Redesign of Object Orientation (1/2)
----------------------------------------

.. admonition:: Language Variant

   Under discussion

* Ada OOP model contains syntactical oddities

   - Relation between type and methods is difficult to track
   - No way to selectively hide/show fields
   - No proper constructor/destructor
   - Calls are not dispatching by default
   - Access type can only access one type in hierarchy by default
   - They propose unique capabilities that have not proven to be necessary

      * Coextensions, controlled types, non-prefix dispatching operator

* Looking at implementing a new model closer to industrial standard

* Both models should be compatible

   - Share same underlying execution model

* Some enahcements can extend to regular records

----------------------------------------
Redesign of Object Orientation (2/2)
----------------------------------------

.. admonition:: Language Variant

   Under discussion

 .. code:: Ada

    package P is
       type Root is class record
          procedure Root (Self : in out T1); -- Constructor
          procedure Root (Self : in out T1; Some_Value : Integer);  -- Constructor

          procedure final (Self : in out T1); -- Destructor

          F : Integer;

          procedure P (Self : in out T2; V : Integer);
       end T2
       with private;

    private

      type Root is class record
         F2 : Integer;

         procedure P2 (Self : in out T2; V : Integer);
       end Root;
    end P;

-----------------------------
Other Topics on the Stove
-----------------------------

.. admonition:: Language Variant

   Under discussion

* Coroutines / Generators / Async-Await
* Improvements around access types
* Initialization verification
* Final classes and packages

