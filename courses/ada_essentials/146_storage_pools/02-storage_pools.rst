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

* Is an :ada:`abstract` type so **you** must create

  * Your own type derived from :ada:`Root_Storage_Pool`
  * Versions of each of the primitive subprograms

-------------------------------------
Primitive Subprogram for Allocation
-------------------------------------

.. container:: latex_environment scriptsize

  .. code:: Ada

    procedure Allocate
      (Pool                     : in out Root_Storage_Pool;
       Storage_Address          : out System.Address;
       Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
       Alignment                : System.Storage_Elements.Storage_Count)
    is abstract;

* Called by runtime when :ada:`new` is performed

* Parameters

  * :ada:`Pool` - specific object keeping track of memory pool
  * :ada:`Storage_Address` - location of allocated memory
  * :ada:`Size_In_Storage_Elements` - number of bytes needed to contain contents
  * :ada:`Alignment` - Byte alignment for memory location

* At a minimum, implementation needs to return address of enough unused
  memory to handle :ada:`Size_In_Storage_Elements` bytes

---------------------------------------
Primitive Subprogram for Deallocation
---------------------------------------

.. container:: latex_environment scriptsize

  .. code:: Ada

    procedure Deallocate
      (Pool                     : in out Root_Storage_Pool;
       Storage_Address          : System.Address;
       Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
       Alignment                : System.Storage_Elements.Storage_Count)
    is abstract;

* Called by runtime when :ada:`new` is performed

* Parameters

  * :ada:`Pool` - specific object keeping track of memory pool
  * :ada:`Storage_Address` - location of memory being freed
  * :ada:`Size_In_Storage_Elements` - number of bytes being freed
  * :ada:`Alignment` - Byte alignment for memory location

* In theory, this doesn't have to do anything

  * In practice, it should return the appropriate number of bytes
    at the address to the "unused" pool

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
