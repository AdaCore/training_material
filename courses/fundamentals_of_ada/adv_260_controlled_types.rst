******************
Controlled Types
******************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

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

==============
Introduction
==============

-------------------------
Constructor / Destructor
-------------------------

* Possible to specify behavior of object initialization, finalization, and assignment

   - Based on type definition
   - Type must derive from `Controlled` or `Limited_Controlled` in package `Ada.Finalization`

* This derived type is called a *controlled type*

    - User may override any or all subprograms in `Ada.Finalization`
    - Default implementation is a null body

==================
Ada.Finalization
==================

---------------
Package Spec
---------------

.. code:: Ada

   package Ada.Finalization is

     type Controlled is abstract tagged private;
     procedure Initialize (Object : in out Controlled)
        is null;
     procedure Adjust    (Object : in out Controlled)
        is null;
     procedure Finalize  (Object : in out Controlled)
        is null;

     type Limited_Controlled is abstract tagged limited private;
     procedure Initialize (Object : in out Limited_Controlled)
        is null;
     procedure Finalize  (Object : in out Limited_Controlled)
        is null;

   private
      -- implementation defined
   end Ada.Finalization;

-------
Uses
-------

* Prevent "resource leak"

   - Logic centralized in service rather than distributed across clients

* Examples: heap reclamation, "mutex" unlocking
* User-defined assignment

----------------
Initialization
----------------

* Subprogram `Initialize` invoked after object created

   - Either by object declaration or allocator
   - Only if no explicit initialization expression

* Often default initialization expressions on record components are sufficient

   - No need for an explicit call to `Initialize`

* Similar to C++ constructor

----------------
Finalization
----------------

* Subprogram `Finalize` invoked just before object is destroyed

   - Leaving the scope of a declared object
   - Unchecked deallocation of an allocated object

* Similar to C++ destructor

------------
Assignment
------------

* Subprogram `Adjust` invoked as part of an assignment operation
* Assignment statement `Target := Source;` is basically:

   - `Finalize (Target)`
   - Copy Source to Target
   - `Adjust (Target)`
   - *Actual rules are more complicated, e.g. to allow cases where Target and Source are the same object*

* Typical situations where objects are access values

   - `Finalize` does unchecked deallocation or decrements a reference count
   - The copy step copies the access value
   - `Adjust` either clones a "deep copy" of the referenced object or increments a reference count

=========
Example
=========

----------------------------------
Unbounded String Via Access Type
----------------------------------

* Type contains a pointer to a string type
* We want the provider to allocate and free memory "safely"

   - No sharing
   - `Adjust` allocates referenced String
   - `Finalize` frees the referenced String
   - Assignment deallocates target string and assigns copy of source string to target string

------------------------
Unbounded String Usage
------------------------

.. code:: Ada

   with Unbounded_String_Pkg; use Unbounded_String_Pkg;
   procedure Test is
      U1 : Ustring_T;
   begin
      U1 := To_Ustring_T ("Hello");
      declare
         U2 : Ustring_T;
      begin
         U2 := To_Ustring_T ("Goodbye");
         U1 := U2; -- Reclaims U1 memory
      end; -- Reclaims U2 memory
   end Test; -- Reclaims U1 memory

-----------------------------
Unbounded String Definition
-----------------------------

.. code:: Ada

   with Ada.Finalization; use Ada.Finalization;
   package Unbounded_String_Pkg is
      -- Implement unbounded strings
      type Ustring_T is private;
      function "=" (L, R : Ustring_T) return Boolean;
      function To_Ustring_T (Item : String) return Ustring_T;
      function To_String (Item : Ustring_T) return String;
      function Length (Item : Ustring_T) return Natural;
      function "&" (L, R : Ustring_T) return Ustring_T;
   private
      type String_Ref is access String;
      type Ustring_T is new Controlled with record
         Ref : String_Ref := new String (1 .. 0);
      end record;
      procedure Finalize (Object : in out Ustring_T);
      procedure Adjust (Object : in out Ustring_T);
   end Unbounded_String_Pkg;

---------------------------------
Unbounded String Implementation
---------------------------------

.. code:: Ada

   with Ada.Unchecked_Deallocation;
   package body Unbounded_String_Pkg is
      procedure Free_String is new Ada.Unchecked_Deallocation
        (String, String_Ref);

      function "=" (L, R : Ustring_T) return Boolean is
         (L.Ref.all = R.Ref.all);

      function To_Ustring_T (Item : String) return Ustring_T is
         (Controlled with Ref => new String'(Item));

      function To_String (Item : Ustring_T) return String is
         (Item.Ref.all);

      function Length (Item : Ustring_T) return Natural is
         (Item.Ref.all'Length);

      function "&" (L, R : Ustring_T) return Ustring_T is
         (Controlled with Ref => new String'(L.Ref.all & R.Ref.all);

      procedure Finalize (Object : in out Ustring_T) is
      begin
         Free_String (Object.Ref);
      end Finalize;

      procedure Adjust (Object : in out Ustring_T) is
      begin
         Object.Ref := new String'(Object.Ref.all);
      end Adjust;
   end Unbounded_String_Pkg;

========
Lab
========

.. include:: labs/adv_260_controlled_types.lab.rst

=========
Summary
=========

---------
Summary
---------

* Controlled types allow access to object construction, assignment, destruction
* `Ada.Finalization` can be expensive to use

   - Other mechanisms may be more efficient

      * But require more rigor in usage
