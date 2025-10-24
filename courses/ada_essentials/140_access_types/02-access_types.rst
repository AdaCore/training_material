==============
Access Types
==============

-------------
Access Type
-------------

* An access type is similar to most other types

  * Keyword :ada:`access` indicates what the access type points to

  .. code:: Ada

     type Rec_T is null record;
     type Rec_Access_T is access Rec_T;
     Rec_Ptr : Rec_Access_T;

* Conversion is **not** possible between pool-specific access types

  .. code:: Ada

     type Rec_Access_2 is access Rec_T;
     Rec_Ptr_2 : Rec_Access_2 := Rec_Access_2 (Rec_Ptr);

  :color-red:`example.adb:6:32: error: target type must be general access type`

-------------
Allocations
-------------

* Objects are created with the :ada:`new` reserved word

  .. code:: Ada

    Rec_Ptr := new Rec_T;

* The created object must be constrained

  * The constraint is given during the allocation

  .. code:: Ada

     type String_Access_T is access all String;
     String_Ptr_1 : String_Access_T := new String(1..10);

* The object can also be created by copying an existing object - using a qualifier

  .. code:: Ada

     String_Ptr_2 : String_Access_T := new String'("abc");

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
      -- create instances of deallocation function
      procedure Free is new Ada.Unchecked_Deallocation
        (Object_T, Access_T);
      Ptr : Access_T := new Object_T;
   begin
      Free (Ptr);
      -- Ptr is now null
   end Proc;
