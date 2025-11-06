==============
Access Types
==============

-------------
Access Type
-------------

* An access type is similar to most other types

  * :ada:`access` indicates what the access type points to

  .. code:: Ada

     type Object_T is null record;
     type Access_T is access Object_T;
     Object_Ptr : Access_T;

* Conversion is **not** possible between this kind of access type

  .. code:: Ada
     :number-lines: 7

     type Other_Access_T is access Object_T;
     Other_Ptr : Other_Access_T := Other_Access_T (Object_Ptr);

  :color-red:`example.adb:8:34: error: target type must be general access type`

.. note::

  A *general access type* is special kind of access type not handled in this
  course. The error message is indicating only those kinds of access types may
  be converted.

-------------
Allocations
-------------

* Objects are created with the :ada:`new` reserved word

  .. code:: Ada

    Object_Ptr := new Object_T;

* The created object must be constrained

  * The constraint is given during the allocation

    .. code:: Ada

      type Unconstrained_String is access String;
      String_Of_10 : Unconstrained_String := new String(1..10);

* The object can also be created by copying an existing object 

  * Using a type qualifier

    .. code:: Ada

      Hello_Ptr   : String_Access_T  := new String'("Hello");
      Integer_Ptr : Integer_Access_T := new Integer'(123);

---------------
Deallocations
---------------

* Deallocations are unsafe

   - Multiple deallocations problems
   - Memory corruptions
   - Access to deallocated objects

* As soon as you use them, you lose the safety of your access
* But sometimes, you have to do what you have to do ...

   - There's no simple way of doing it
   - Ada provides `Ada.Unchecked_Deallocation`
   - Has to be instantiated (it's a generic)
   - Works on an object, reset to :ada:`null` afterwards

----------------------
Deallocation Example
----------------------

.. code:: Ada

   -- generic used to deallocate memory
   with Ada.Unchecked_Deallocation;
   procedure Proc is
      type Object_T is null record;
      type Access_T is access Object_T;
      Ptr : Access_T := new Object_T;

      -- create instances of deallocation function
      procedure Free is new Ada.Unchecked_Deallocation
        (Object_T, Access_T);
   begin
      Free (Ptr);
      -- Ptr is now null
   end Proc;
