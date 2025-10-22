======================
System.Storage_Pools
======================

---------------------------
System.Storage_Pools Spec
---------------------------

.. code:: Ada

  package System.Storage_Pools
    with Pure
  is
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

-------------------
Storage Pool Type
-------------------

:ada:`Root_Storage_Pool`

* Abstract type for tracking internal (and user-specified if desired) data

* Uses :ada:`Ada.Finalization.Limited_Controlled`

  * :ada:`Finalization` objects call primitives on construction, destruction, and modification
  * :ada:`Limited_Controlled` makes the type :ada:`limited`

---------------------
Allocate/Deallocate
---------------------

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

* :ada:`Allocate` called when :ada:`new` is used for allocation
* :ada:`Deallocate` called by :ada:`Unchecked_Deallocation`
* Runtime is responsible for passing in parameters

  * Implementation is responsible for handling them correctly

--------------------
Storage_Size Query
--------------------

.. code:: Ada

  function Storage_Size
    (Pool : Root_Storage_Pool)
     return System.Storage_Elements.Storage_Count
  is abstract;

* Query function available to application to check memory availability

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
