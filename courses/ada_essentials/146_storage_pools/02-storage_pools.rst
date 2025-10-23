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

* Type for tracking internal (and user-specified if desired) data

* Uses :ada:`Ada.Finalization.Limited_Controlled`

  * :ada:`Finalization` objects call primitives on construction, destruction, and modification
  * :ada:`Limited_Controlled` makes the type :ada:`limited`

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

-------------------------------
Implementation Example - Spec
-------------------------------

.. container:: source_include 146_storage_pools/examples/storage_pools/memory_mgmt.ads code:Ada

-------------------------------------------
Implementation Example - Helper Functions
-------------------------------------------

.. container:: source_include 146_storage_pools/examples/storage_pools/memory_mgmt.adb :start-after:helpers_begin :end-before:helpers_end code:Ada

----------------------------------------
Implementation Example - External APIs
----------------------------------------

.. container:: source_include 146_storage_pools/examples/storage_pools/memory_mgmt.adb :start-after:external_apis_begin :end-before:external_apis_end code:Ada

--------------------------------
Implementation Example - Usage
--------------------------------

.. container:: source_include 146_storage_pools/examples/storage_pools/integer_list.ads code:Ada

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
