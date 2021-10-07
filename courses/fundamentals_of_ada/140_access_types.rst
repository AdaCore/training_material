
**************
Access Types
**************

.. role:: ada(code)
    :language: Ada

==============
Introduction
==============

---------------------
Access Types Design
---------------------

* Memory addresses objects are called **access types**
* Objects are associated to **pools** of memory

  - With different allocation / deallocation policies

* Access objects are **guaranteed** to always be meaningful

  - In the absence of :ada:`Unchecked_Deallocation`
  - And if pool-specific

.. container:: columns

 .. container:: column

  .. code:: Ada

     type Integer_Pool_Access
       is access Integer;
     P_A : Integer_Pool_Access
       := new Integer;

     type Integer_General_Access is access all Integer;
     G : aliased Integer
     G_A : Integer_General_Access := G'access;

 .. container:: column

  .. code:: C++

     int * P_C = malloc (sizeof (int));
     int * P_CPP = new int;
     int * G_C = &Some_Int;
.

-------------------------------
Access Types Can Be Dangerous
-------------------------------

* Multiple memory issues

   - Leaks / corruptions

* Introduces potential random failures complicated to analyze
* Increase the complexity of the data structures
* May decrease the performances of the application

   - Dereferences are slightly more expensive than direct access
   - Allocations are a lot more expensive than stacking objects

* Ada avoids using accesses as much as possible

   - Arrays are not pointers
   - Parameters are implicitly passed by reference

* Only use them when needed

---------------
Stack vs Heap
---------------

.. code:: Ada

  I : Integer := 0;
  J : String := "Some Long String";

.. image:: items_on_stack.png
   :width: 50%

.. code:: Ada

  I : Access_Int:= new Integer'(0);
  J : Access_Str := new String ("Some Long String");

.. image:: stack_pointing_to_heap.png
   :width: 50%

==========================
Access Types
==========================

----------
Examples
----------

.. include:: examples/140_access_types/access_types.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/140_access_types.html#access-types`

----------------------
Declaration Location
----------------------

* Can be at library level

   .. code:: Ada

      package P is
        type String_Access is access String;
      end P;

* Can be nested in a procedure

   .. code:: Ada

      package body P is
         procedure Proc is
            type String_Access is access String;
         begin
            ...
         end Proc;
      end P;

* Nesting adds non-trivial issues

   - Creates a nested pool with a nested accessibility
   - Don't do that unless you know what you are doing! (see later)

-------------
Null Values
-------------

* A pointer that does not point to any actual data has a null value
* Without an initialization, a pointer is :ada:`null` by default
* :ada:`null` can be used in assignments and comparisons

.. code:: Ada

   declare
      type Acc is access all Integer;
      V : Acc;
   begin
      if V = null then
         --  will go here
      end if
      V := new Integer'(0);
      V := null; -- semantically correct, but memory leak

---------------------------
Access Types and Primitives
---------------------------

* Subprogram using an access type are primitive of the **access type**

    - **Not** the type of the accessed object

   .. code:: Ada

         type A_T is access all T;
         procedure Proc (V : A_T); -- Primitive of A_T, not T

* Primitive of the type can be created with the :ada:`access` mode

    - **Anonymous** access type

   .. code:: Ada

         procedure Proc (V : access T); -- Primitive of T

------------------------
Dereferencing Pointers
------------------------

* :ada:`.all` does the access dereference

   - Lets you access the object pointed to by the pointer

* :ada:`.all` is optional for

   - Access on a component of an array
   - Access on a component of a record

----------------------
Dereference Examples
----------------------

.. code:: Ada

   type R is record
     F1, F2 : Integer;
   end record;
   type A_Int is access Integer;
   type A_String is access all String;
   type A_R is access R;
   V_Int    : A_Int := new Integer;
   V_String : A_String := new String'("abc");
   V_R      : A_R := new R;

.. code:: Ada

   V_Int.all := 0;
   V_String.all := "cde";
   V_String (1) := 'z'; -- similar to V_String.all (1) := 'z';
   V_R.all := (0, 0);
   V_R.F1 := 1; -- similar to V_R.all.F1 := 1;

===========================
Pool-Specific Access Types
===========================

----------
Examples
----------

.. include:: examples/140_access_types/pool_specific_access_types.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/140_access_types.html#pool-specific-access-types`

---------------------------
Pool-Specific Access Type
---------------------------

* An access type is a type

   .. code:: Ada

      type T is [...]
      type T_Access is access T;
      V : T_Access := new T;

* Conversion is **not** possible between pool-specific access types

-------------
Allocations
-------------

* Objects are created with the :ada:`new` reserved word
* The created object must be constrained

   - The constraint is given during the allocation

      .. code:: Ada

         V : String_Access := new String (1 .. 10);

* The object can be created by copying an existing object - using a qualifier

   .. code:: Ada

      V : String_Access := new String'("This is a String");

---------------
Deallocations
---------------

* Deallocations are unsafe

   - Multiple deallocations problems
   - Memory corruptions
   - Access to deallocated objects

* As soon as you use them, you lose the safety of your pointers
* But sometimes, you have to do what you have to do ...

   - There's no simple way of doing it
   - Ada provides `Ada.Unchecked_Deallocation`
   - Has to be instantiated (it's a generic)
   - Must work on an object, reset to :ada:`null` afterwards

----------------------
Deallocation Example
----------------------

.. code:: Ada

   -- generic used to deallocate memory
   with Ada.Unchecked_Deallocation;
   procedure P is
      type An_Access is access A_Type;
      -- create instances of deallocation function
      -- (object type, access type)
      procedure Free is new Ada.Unchecked_Deallocation
        (A_Type, An_Access);
      V : An_Access := new A_Type;
   begin
      Free (V);
      -- V is now null
   end P;

==========================
General Access Types
==========================

----------
Examples
----------

.. include:: examples/140_access_types/general_access_types.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/140_access_types.html#general-access-types`

----------------------
General Access Types
----------------------

* Can point to any pool (including stack)

   .. code:: Ada

      type T is [...]
      type T_Access is access all T;
      V : T_Access := new T;

* Still distinct type
* Conversions are possible

   .. code:: Ada

      type T_Access_2 is access all T;
      V2 : T_Access_2 := T_Access_2 (V); -- legal

-----------------------
Referencing The Stack
-----------------------

* By default, stack-allocated objects cannot be referenced - and can even be optimized into a register by the compiler
* :ada:`aliased` declares an object to be referenceable through an access value

   .. code:: Ada

      V : aliased Integer;

* :ada:`'Access` attribute gives a reference to the object

   .. code:: Ada

      A : Int_Access := V'Access;

   - :ada:`'Unchecked_Access` does it **without checks**

----------------------------
`Aliased` Objects Examples
----------------------------

.. code:: Ada

   type Acc is access all Integer;
   V, G : Acc;
   I : aliased Integer;
   ...
   V := I'Access;
   V.all := 5; -- Same a I := 5
   ...
   procedure P1 is
      I : aliased Integer;
   begin
      G := I'Unchecked_Access;
   end P1;

   procedure P2 is
   begin
      G.all := 5;
      -- What if P2 is called after P1?
   end P2;

------
Quiz
------

.. code:: Ada

   type One_T is access all Integer;
   type Two_T is access Integer;

   A : aliased Integer;
   B : Integer;

   One : One_T;
   Two : Two_T;

Which assignment is legal?

A. ``One := B'Access;``
B. :answermono:`One := A'Access;`
C. ``Two := B'Access;``
D. ``Two := A'Access;``

.. container:: animate

   :ada:`'Access` is only allowed for general access types
   (:ada:`One_T`). To use :ada:`'Access` on an object, the
   object must be :ada:`aliased`.

======================
Accessibility Checks
======================

----------
Examples
----------

.. include:: examples/140_access_types/accessibility_checks.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/140_access_types.html#accessibility-checks`

--------------------------------------------
Introduction to Accessibility Checks (1/2)
--------------------------------------------

* The depth of an object depends on its nesting within declarative scopes

   .. code:: Ada

      package body P is
         --  Library level, depth 0
         procedure Proc is
            --  Library level subprogram, depth 1
            procedure Nested is
               -- Nested subprogram, enclosing + 1, here 2
            begin
                null;
            end Nested;
         begin
            null;
         end Proc;
      end P;

* Access types can access objects at most of the same depth
* The compiler checks it statically

   - Removing checks is a workaround!

--------------------------------------------
Introduction to Accessibility Checks (2/2)
--------------------------------------------

.. code:: Ada

   package body P is
      type T0 is access all Integer;
      A0 : T0;
      V0 : aliased Integer;
      procedure Proc is
         type T1 is access all Integer;
         A1 : T1;
         V1 : aliased Integer;
      Begin
         A0 := V0'Access;
         A0 := V1'Access; -- illegal
         A0 := V1'Unchecked_Access;
         A1 := V0'Access;
         A1 := V1'Access;
         A1 := T1 (A0);
         A0 := T0 (A1); -- illegal
         A1 := new Integer;
         A0 := T0 (A1); -- illegal
     end Proc;
   end P;

* To avoid having to face these issues, avoid nested access types

-------------------------------------
Getting Around Accessibility Checks
-------------------------------------

* Sometimes it is OK to use unsafe accesses to data
* :ada:`'Unchecked_Access` allows access to a variable of an incompatible accessibility level
* Beware of potential problems!

   .. code:: Ada

      type Acc is access all Integer;
      G : Acc;
      procedure P is
         V : aliased Integer;
      begin
         G := V'Unchecked_Access;
         ...
         Do_Something ( G.all ); -- This is "reasonable"
      end P;

.. container:: speakernote

   Not the best way to write code

-----------------------------------------
Using Pointers For Recursive Structures
-----------------------------------------

* It is not possible to declare recursive structure
* But there can be an access to the enclosing type

.. code:: Ada

   type Cell; -- partial declaration
   type Cell_Access is access all Cell;
   type Cell is record -- full declaration
      Next       : Cell_Access;
      Some_Value : Integer;
   end record;

------
Quiz
------

.. code:: Ada

   type Global_Access_T is access all Integer;
   Global_Pointer : Global_Access_T;
   Global_Object  : aliased Integer;
   procedure Proc_Access is
      type Local_Access_T is access all Integer;
      Local_Pointer : Local_Access_T;
      Local_Object  : aliased Integer;
   begin

Which assignment is illegal?

A. ``Global_Pointer := Global_Object'Access;``
B. :answermono:`Global_Pointer := Local_Object'Access;`
C. ``Local_Pointer  := Global_Object'Access;``
D. ``Local_Pointer  := Local_Object'Access;``

.. container:: animate

   Explanations

   A. Pointer type has same depth as object
   B. Pointer type is not allowed to have higher level than pointed-to object
   C. Pointer type has lower depth than pointed-to object
   D. Pointer type has same depth as object

===================
Memory Management
===================

----------
Examples
----------

.. include:: examples/140_access_types/memory_management.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/140_access_types.html#memory-management`

------------------------------
Common Memory Problems (1/3)
------------------------------

* Uninitialized pointers

   .. code:: Ada

      declare
         type An_Access is access all Integer;
         V : An_Access;
      begin
         V.all := 5; -- constraint error

* Double deallocation

   .. code:: Ada

      declare
         type An_Access is access all Integer;
         procedure Free is new
            Ada.Unchecked_Deallocation (Integer, An_Access);
         V1 : An_Access := new Integer;
         V2 : An_Access := V1;
      begin
         Free (V1);
         ...
         Free (V2);

   - May raise :ada:`Storage_Error` if memory is still protected (unallocated)
   - May deallocate a different object if memory has been reallocated

      + Putting that object in an inconsistent state

------------------------------
Common Memory Problems (2/3)
------------------------------

* Accessing deallocated memory

   .. code:: Ada

      declare
         type An_Access is access all Integer;
         procedure Free is new
            Ada.Unchecked_Deallocation (Integer, An_Access);
         V1 : An_Access := new Integer;
         V2 : An_Access := V1;
      begin
         Free (V1);
         ...
         V2.all := 5;

   - May raise :ada:`Storage_Error` if memory is still protected (unallocated)
   - May modify a different object if memory has been reallocated (putting that object in an inconsistent state)

------------------------------
Common Memory Problems (3/3)
------------------------------

* Memory leaks

   .. code:: Ada

      declare
         type An_Access is access all Integer;
         procedure Free is new
            Ada.Unchecked_Deallocation (Integer, An_Access);
         V : An_Access := new Integer;
      begin
         V := null;

   - Silent problem

      + Might raise :ada:`Storage_Error` if too many leaks
      + Might slow down the program if too many page faults

-----------------------------
How To Fix Memory Problems?
-----------------------------

* There is no language-defined solution
* Use the debugger!
* Use additional tools

   - :command:`gnatmem`  monitor memory leaks
   - :command:`valgrind`  monitor all the dynamic memory
   - `GNAT.Debug_Pools` gives a pool for an access type, raising explicit exception in case of invalid access
   - Others...

========================
Anonymous Access Types
========================

----------
Examples
----------

.. include:: examples/140_access_types/anonymous_access_types.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/140_access_types.html#anonymous-access-types`

-----------------------------
Anonymous Access Parameters
-----------------------------

* Parameter modes are of 4 types: :ada:`in`, :ada:`out`, :ada:`in out`, :ada:`access`
* The access mode is called **anonymous access type**

   - Anonymous access is implicitly general (no need for :ada:`all`)

* When used:

   - Any named access can be passed as parameter
   - Any anonymous access can be passed as parameter

.. code:: Ada

   type Acc is access all Integer;
   G : Acc := new Integer;
   procedure P1 (V : access Integer);
   procedure P2 (V : access Integer) is
   begin
      P1 (G);
      P1 (V);
   end P;

------------------------
Anonymous Access Types
------------------------

* Other places can declare an anonymous access

   .. code:: Ada

      function F return access Integer;
      V : access Integer;
      type T (V : access Integer) is record
        C : access Integer;
      end record;
      type A is array (Integer range <>) of access Integer;

* Do not use them without a clear understanding of accessibility check rules

----------------------------------
Anonymous Access Constants
----------------------------------

* :ada:`constant` (instead of :ada:`all`) denotes an access type through which the referenced object cannot be modified

   .. code:: Ada

      type CAcc is access constant Integer;
      G1 : aliased Integer;
      G2 : aliased constant Integer;
      V1 : CAcc := G1'Access;
      V2 : CAcc := G2'Access;
      V1.all := 0; -- illegal

* :ada:`not null` denotes an access type for which null value cannot be accepted

   - Available in Ada 2005 and later

   .. code:: Ada

      type NAcc is not null access Integer;
      V : NAcc := null; -- illegal

* Also works for subprogram parameters

   .. code:: Ada

      procedure Bar ( V1 : access constant integer);
      procedure Foo ( V1 : not null access integer); -- Ada 2005

========
Lab
========

.. include:: labs/140_access_types.lab.rst

=========
Summary
=========

---------
Summary
---------

* Access types are the same as C/C++ pointers
* There are usually better ways of memory management

   - Language has its own ways with dealing with large objects passed as parameters
   - Language has libraries dedicated to memory allocation / deallocation

* At a minimum, create your own generics to do allocation / deallocation

   - Minimize memory leakage and corruption
