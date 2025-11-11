======================
Storage Pool Example
======================

--------------------------------
Creating Your Own Storage Pool
--------------------------------

* To create your own storage pool API you must

  * Create a concrete type for the storage pool
  * Create concrete versions of the abstract primitive subprograms
  * Implement some mechanism for allocating/freeing memory

* This chapter will build the storage pool in package :ada:`Memory_Mgmt` and show a usage in :ada:`Integer_List`

.. note:: The code in this chapter is not complete - it is missing things like dependency clauses and scoping constructs.

---------------------------------
Create Concrete Type/Primitives
---------------------------------

* In the spec for :ada:`Memory_Mgmt`, you need to create the type and primitives

.. container:: source_include 146_storage_pools/examples/storage_pool_example/memory_mgmt.ads code:Ada :start-after:concrete_start :end-before:concrete_end

------------------------------------
Implement Mechanism - Storage Pool
------------------------------------

* In addition, you need to actually create a visible object in the spec to be used for the storage pool

.. container:: source_include 146_storage_pools/examples/storage_pool_example/memory_mgmt.ads :start-after:implementation_begin :end-before:implementation_end code:Ada

* In our case, we also add an API to allow us to print information about memory usage

----------------------------
Implement Mechanism - Data
----------------------------

* Our "storage pool" is made up of

  * Large array of bytes (:ada:`Memory_Block`)
  * Array indicating which bytes are in use (:ada:`Memory_Used`)

* We also will track

  * Current amount of memory being used (:ada:`Current_Water_Mark`)
  * Largest amount of memory ever used (:ada:`High_Water_Mark`)

.. container:: source_include 146_storage_pools/examples/storage_pool_example/memory_mgmt.adb :start-after:objects_begin :end-before:objects_end code:Ada

--------------------------------------
Implement Mechanism - Reserve Memory
--------------------------------------

* We need a subprogram to

  * Reserve a block of memory
  * Maintain our tracking data

.. container:: source_include 146_storage_pools/examples/storage_pool_example/memory_mgmt.adb :start-after:set_in_use_begin :end-before:set_in_use_end code:Ada

-------------------------------------
Implement Mechanism - Locate Memory
-------------------------------------

* We also need a subprogram to find a sequence of bytes large enough to accomodate our request

.. container:: source_include 146_storage_pools/examples/storage_pool_example/memory_mgmt.adb :start-after:find_free_block_begin :end-before:find_free_block_end code:Ada

------------------------------------------
Implementation Mechanism - External APIs
------------------------------------------

* Then, we need to implement the external APIs for the storage pool

  * :ada:`Allocate` - find a block of bytes as large as we need

    * Flag those bytes as used
    * Return the address of the beginning of the block

  * :ada:`Deallocate` - clear flag for each byte we are releasing

  * :ada:`Storage_Size` - return amount of memory in use

.. container:: source_include 146_storage_pools/examples/storage_pool_example/memory_mgmt.adb :start-after:external_apis_begin :end-before:external_apis_end code:Ada

-----------------------------------
Implement Mechanism - Diagnostics
-----------------------------------

* We also need a subprogram to find a sequence of bytes large enough to accomodate our request

.. container:: source_include 146_storage_pools/examples/storage_pool_example/memory_mgmt.adb :start-after:diagnostics_begin :end-before:diagnostics_end code:Ada

--------------------------------
Implementation Example - Usage
--------------------------------

* Example of using our storage pool when implementing a linked list of integers

  * Note line 15

.. container:: source_include 146_storage_pools/examples/storage_pool_example/integer_list.ads code:Ada :number-lines:1
