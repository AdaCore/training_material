***********************
Advanced Access Types
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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

---------------------
Access Types Design
---------------------

* Memory addresses objects are called :dfn:`access types`
* Objects are associated to **pools** of memory

  - With different allocation / deallocation policies

.. code:: Ada

     type Integer_Pool_Access is access Integer;
     P_A : Integer_Pool_Access := new Integer;

     type Integer_General_Access is access all Integer;
     G : aliased Integer;
     G_A1 : Integer_General_Access := G'Access;
     G_A2 : Integer_General_Access := new Integer;

* This module is mostly about :dfn:`general access types`

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
   - Many parameters are implicitly passed by reference

* Only use them when needed

==========================
Access Types
==========================

----------------------
Declaration Location
----------------------

* Can be at library level

   .. code:: Ada

      package P is
        type String_Access is access all String;
      end P;

* Can be nested in a procedure

   .. code:: Ada

      package body P is
         procedure Proc is
            type String_Access is access all String;
         begin
            ...
         end Proc;
      end P;

* Nesting adds non-trivial issues

   - Creates a nested pool with a nested accessibility
   - Don't do that unless you know what you are doing! (see later)

---------------------------
Access Types and Primitives
---------------------------

* Subprograms using an access type are primitive of the **access type**

    - **Not** the type of the accessed object

   .. code:: Ada

         type A_T is access all T;
         procedure Proc (V : A_T); -- Primitive of A_T, not T

* Primitive of the type can be created with the :ada:`access` mode

    - **Anonymous** access type

   .. code:: Ada

         procedure Proc (V : access T); -- Primitive of T

-----------------------
Anonymous Access Types
-----------------------

* Can be declared in several places

    + Are **general**

* Make sense as parameters of a primitive
* Else, raises a fundamental issue

    + Two different :ada:`access T` are **not** compatible

.. include:: examples/140_access_types/anonymous_access/src/main.adb
    :code: Ada

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

======================
Accessibility Checks
======================

--------------------------------------------
Introduction to Accessibility Checks (1/2)
--------------------------------------------

* The depth of an object depends on its nesting within declarative scopes

   .. code:: Ada

      package body P is
         --  Library level, depth 0
         O0 : aliased Integer;
         procedure Proc is
            --  Library level subprogram, depth 1
            type Acc1 is access all Integer;
            procedure Nested is
               -- Nested subprogram, enclosing + 1, here 2
               O2 : aliased Integer;

* Access **types** can only access objects that are at **same or lower depth**
* :ada:`type Acc1` (depth 1) can access :ada:`O0` (depth 0) but not `O2` (depth 2)
* The compiler checks it statically

   - Removing checks is a workaround!

* Note: Subprogram library units are at **depth 1** and not 0

--------------------------------------------
Introduction to Accessibility Checks (2/2)
--------------------------------------------

* Issues with nesting

.. include:: examples/140_access_types/nesting_issues/src/p.adb
    :code: Ada

* Simple workaround is to avoid nested access types

------------------------------
Dynamic Accessibility Checks
------------------------------

* Following the same rules

    - Performed dynamically by the runtime

* Lots of possible cases

    - New compiler versions may detect more cases
    - Using access always requires proper debugging and reviewing

.. include:: examples/140_access_types/dynamic_accessibility_check/src/main.adb
    :code: Ada

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

===================
Memory Corruption
===================

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

===================
Memory Management
===================

--------------------
Simple Linked List
--------------------

* A linked list object typically consists of:

  * Content
  * "Indication" of next item in list

    * Fancier linked lists may reference previous item in list

* "Indication" is just a pointer to another linked list object

  * Therefore, self-referencing

* Ada does not allow a record to self-reference

------------------
Incomplete Types
------------------

* In Ada, an :dfn:`incomplete type` is just the word :ada:`type` followed by the type name

  * Optionally, the name may be followed by :ada:`(<>)` to indicate the full type may be unconstrained

* Ada allows access types to point to an incomplete type

  * Just about the only thing you *can* do with an incomplete type!

.. code:: Ada

   type Some_Record_T;
   type Some_Record_Access_T is access all Some_Record_T;

   type Unconstrained_Record_T (<>);
   type Unconstrained_Record_Access_T is access all Unconstrained_Record_T;

   type Some_Record_T is record
      Field : String (1 .. 10);
   end record;

   type Unconstrained_Record_T (Size : Index_T) is record
      Field : String (1 .. Size);
   end record;
   
--------------------
Linked List in Ada
--------------------

* Now that we have a pointer to the record type (by name), we can use it in the full definition of the record type

.. code:: Ada

   type Some_Record_T is record
      Field : String (1 .. 10);
      Next  : Some_Record_Access_T;
   end record;

   type Unconstrained_Record_T (Size : Index_T) is record
      Field    : String (1 .. Size);
      Next     : Unconstrained_Record_Access_T;
      Previous : Unconstrained_Record_Access_T;
   end record;

------------------------
Simplistic Linked List
------------------------

.. code:: Ada

  with Ada.Text_IO; use Ada.Text_IO;
  with Ada.Unchecked_Deallocation;
  procedure Simple is
     type Some_Record_T;
     type Some_Record_Access_T is access all Some_Record_T;
     type Some_Record_T is record
        Field : String (1 .. 10);
        Next  : Some_Record_Access_T;
     end record;
  
     Head : Some_Record_Access_T := null;
     Item : Some_Record_Access_T := null;
  
     Line : String (1 .. 10);
     Last : Natural;

   procedure Free is new Ada.Unchecked_Deallocation
     (Some_Record_T, Some_Record_Access_T);
  
  begin
  
     loop
        Put ("Enter String: ");
        Get_Line (Line, Last);
        exit when Last = 0;
        Line (Last + 1 .. Line'last) := (others => ' ');
        Item                         := new Some_Record_T;
        Item.all                     := (Line, Head);
        Head                         := Item;
     end loop;
  
     Put_Line ("List");
     while Head /= null loop
        Put_Line ("  " & Head.Field);
        Head := Head.Next;
     end loop;

     Put_Line ("Delete");
     Free (Item);
     GNAT.Debug_Pools.Print_Info_Stdout (Storage_Pool);
  
  end Simple;

==================
Memory Debugging
==================

------------------
GNAT.Debug_Pools
------------------

* Ada allows the coder to specify *where* the allocated memory comes from

  * Called :dfn:`Storage Pool`
  * Basically, connecting :ada:`new` and :ada:`Unchecked_Deallocation` with some other code
  * More details in the next section

  .. container:: latex_environment footnotesize

    .. code:: Ada

      type Linked_List_Ptr_T is access all Linked_List_T;
      for Linked_List_Ptr_T'storage_pool use Memory_Mgmt.Storage_Pool;

* GNAT uses this mechanism in the run-time package :ada:`GNAT.Debug_Pools` to track allocation/deallocation

  .. container:: latex_environment footnotesize

    .. code:: Ada

      with GNAT.Debug_Pools;
      package Memory_Mgmt is
        Storage_Pool : GNAT.Debug_Pools.Debug_Pool;
      end Memory_Mgmt;

---------------------------------
GNAT.Debug_Pools Spec (Partial)
---------------------------------

.. code:: Ada

  package GNAT.Debug_Pools is
  
     type Debug_Pool is new System.Checked_Pools.Checked_Pool with private;
  
     generic
        with procedure Put_Line (S : String) is <>;
        with procedure Put      (S : String) is <>;
     procedure Print_Info
       (Pool          : Debug_Pool;
        Cumulate      : Boolean := False;
        Display_Slots : Boolean := False;
        Display_Leaks : Boolean := False);
  
     procedure Print_Info_Stdout
       (Pool          : Debug_Pool;
        Cumulate      : Boolean := False;
        Display_Slots : Boolean := False;
        Display_Leaks : Boolean := False);
     --  Standard instantiation of Print_Info to print on standard_output.
  
     procedure Dump_Gnatmem (Pool : Debug_Pool; File_Name : String);
     --  Create an external file on the disk, which can be processed by gnatmem
     --  to display the location of memory leaks.
  
     procedure Print_Pool (A : System.Address);
     --  Given an address in memory, it will print on standard output the known
     --  information about this address
  
     function High_Water_Mark
       (Pool : Debug_Pool) return Byte_Count;
     --  Return the highest size of the memory allocated by the pool.
  
     function Current_Water_Mark
       (Pool : Debug_Pool) return Byte_Count;
     --  Return the size of the memory currently allocated by the pool.
  
  private
     -- ...
  end GNAT.Debug_Pools;

------------------------------
Displaying Debug Information
------------------------------

* Simple modifications to our linked list example

  * Create and use storage pool

    .. container:: latex_environment footnotesize

      `` ``

      .. code:: Ada

        with GNAT.Debug_Pools; -- Added
        procedure Simple is
           Storage_Pool : GNAT.Debug_Pools.Debug_Pool; -- Added
           type Some_Record_T;
           type Some_Record_Access_T is access all Some_Record_T;
           for Some_Record_Access_T'storage_pool
               use Storage_Pool; -- Added

      `` ``

  * Dump info after each :ada:`new`

    .. container:: latex_environment footnotesize

      `` ``

      .. code:: Ada

        Item                         := new Some_Record_T;
        GNAT.Debug_Pools.Print_Info_Stdout (Storage_Pool); -- Added
        Item.all := (Line, Head);

      `` ``

  * Dump info after :ada:`free`

    .. container:: latex_environment footnotesize

      `` ``

      .. code:: Ada

        Free (Item);
        GNAT.Debug_Pools.Print_Info_Stdout (Storage_Pool); -- Added
  
-------------------
Execution Results
-------------------

::

  Enter String: X
  Total allocated bytes :  24
  Total logically deallocated bytes :  0
  Total physically deallocated bytes :  0
  Current Water Mark:  24
  High Water Mark:  24

  Enter String: Y
  Total allocated bytes :  48
  Total logically deallocated bytes :  0
  Total physically deallocated bytes :  0
  Current Water Mark:  48
  High Water Mark:  48

  Enter String:
  List
    Y
    X
  Delete
  Total allocated bytes :  48
  Total logically deallocated bytes :  24
  Total physically deallocated bytes :  0
  Current Water Mark:  24
  High Water Mark:  48

================
Memory Control
================

----------------------
System.Storage_Pools
----------------------

* Mechanism to allow coder control over allocation/deallocation process

  * Uses :ada:`Ada.Finalization.Limited_Controlled` to implement customized memory allocation and deallocation.
  * Must be specified for each access type being controlled

    .. code:: Ada

      type Boring_Access_T is access Some_T;
      -- Storage Pools mechanism not used here
      type Important_Access_T is access Some_T;
      for Important_Access_T'storage_pool use My_Storage_Pool;
      -- Storage Pools mechanism used for Important_Access_T


-------------------------------------
System.Storage_Pools Spec (Partial)
-------------------------------------

.. code:: Ada

  with Ada.Finalization;
  with System.Storage_Elements;
  package System.Storage_Pools with Pure is
    type Root_Storage_Pool is abstract
      new Ada.Finalization.Limited_Controlled with private;
    pragma Preelaborable_Initialization (Root_Storage_Pool);

    procedure Allocate
      (Pool                     : in out Root_Storage_Pool;
       Storage_Address          : out System.Address;
       Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
       Alignment                : System.Storage_Elements.Storage_Count)
    is abstract;

    procedure Deallocate
      (Pool                     : in out Root_Storage_Pool;
       Storage_Address          : System.Address;
       Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
       Alignment                : System.Storage_Elements.Storage_Count)
    is abstract;

    function Storage_Size
      (Pool : Root_Storage_Pool)
       return System.Storage_Elements.Storage_Count
    is abstract;

  private
    -- ...
  end System.Storage_Pools;

-----------------------------------
System.Storage_Pools Explanations
-----------------------------------

* Note :ada:`Root_Storage_Pool`, :ada:`Allocate`, :ada:`Deallocate`, and :ada:`Storage_Size` are :ada:`abstract`

  * You must create your own type derived from :ada:`Root_Storage_Pool`
  * You must create versions of :ada:`Allocate`, :ada:`Deallocate`, and :ada:`Storage_Size` to allocate/deallocate memory

* Parameters

    * :ada:`Pool`

      * Memory pool being manipulated

    * :ada:`Storage_Address`

      * For :ada:`Allocate` - location in memory where access type will point to
      * For :ada:`Deallocate` - location in memory where memory should be released

    * :ada:`Size_In_Storage_Elements`

      * Number of bytes needed to contain contents

    * :ada:`Alignment`

      * Byte alignment for memory location

----------------------------------------
System.Storage_Pools Example (Partial)
----------------------------------------

.. code:: Ada

   subtype Index_T is Storage_Count range 1 .. 1_000;
   Memory_Block : aliased array (Index_T) of Interfaces.Unsigned_8;
   Memory_Used  : array (Index_T) of Boolean := (others => False);

   procedure Set_In_Use (Start  : Index_T;
                         Length : Storage_Count;
                         Used   : Boolean);

   function Find_Free_Block (Length : Storage_Count) return Index_T;

   procedure Allocate
     (Pool                     : in out Storage_Pool_T;
      Storage_Address          :    out System.Address;
      Size_In_Storage_Elements :        Storage_Count;
      Alignment                :        Storage_Count) is
      Index : Storage_Count := Find_Free_Block (Size_In_Storage_Elements);
   begin
      Storage_Address := Memory_Block (Index)'address;
      Set_In_Use (Index, Size_In_Storage_Elements, True);
   end Allocate;

   procedure Deallocate
     (Pool                     : in out Storage_Pool_T;
      Storage_Address          :        System.Address;
      Size_In_Storage_Elements :        Storage_Count;
      Alignment                :        Storage_Count) is
   begin
      for I in Memory_Block'range loop
         if Memory_Block (I)'address = Storage_Address then
            Set_In_Use (I, Size_In_Storage_Elements, False);
         end if;
      end loop;
   end Deallocate;

========
Lab
========

.. include:: labs/adv_140_access_types.lab.rst

=========
Summary
=========

---------
Summary
---------

* Access types when used with "dynamic" memory allocation can cause problems

  * Whether actually dynamic or using managed storage pools, memory leaks/lack can occur
  * Storage pools can help diagnose memory issues, but it's still a usage issue

* :ada:`GNAT.Debug_Pools` is useful for debugging memory issues

  * Mostly in low-level testing
  * Could integrate it with an error logging mechanism

* :ada:`System.Storage_Pools` can be used to control memory usage

  * Adds overhead
