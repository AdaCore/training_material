======================
System.Storage_Pools
======================

-------------------
Storage Pool Type
-------------------

.. code:: Ada

   type Root_Storage_Pool is abstract
      new Ada.Finalization.Limited_Controlled with private;
   pragma Preelaborable_Initialization (Root_Storage_Pool);

* Type for tracking internal (or user-specified) data

* Uses :ada:`Ada.Finalization.Limited_Controlled`

  * :ada:`Finalization` objects call primitives on construction, destruction, and modification
  * :ada:`Limited_Controlled` makes the type :ada:`limited`

* Is an :ada:`abstract` type

  * You must create your own type derived from :ada:`Root_Storage_Pool`
  * You must create versions of each of the primitive subprograms (shown on the next slide)

--------------------------
Storage Pool Subprograms
--------------------------

.. code:: Ada

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

* :ada:`Allocate` called when :ada:`new` is used for allocation
* :ada:`Deallocate` called by :ada:`Unchecked_Deallocation`
* :ada:`Storage_Size` reports total amount of memory being managed

* Runtime is responsible for passing in parameters

  * Implementation is responsible for handling them correctly

------------------------------------
Storage Pool Subprogram Parameters
------------------------------------

* Parameters

  * :ada:`Pool`

    * Memory pool being manipulated

  * :ada:`Storage_Address`

    * :ada:`Allocate` - location in memory where access type will point to
    * :ada:`Deallocate` - location in memory where memory should be released

  * :ada:`Size_In_Storage_Elements`

    * Number of bytes needed to contain contents

  * :ada:`Alignment`

    * Byte alignment for memory location

----------------------
Limitations/Benefits
----------------------

* Static lifetime

  * Pool object must outlive all allocations from it

* No automatic collection

  * Freeing is still manual (unless you design otherwise)

* :ada:`Storage_Size` can limit how many objects you can allocate

* Safety

  * Still type-safe
  * Ada enforces pool–access type consistency
