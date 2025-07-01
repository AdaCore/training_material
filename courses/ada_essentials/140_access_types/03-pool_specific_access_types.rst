===========================
Pool-Specific Access Types
===========================

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

* As soon as you use them, you lose the safety of your access
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

