========
Lab
========

-------------------
Storage Pools Lab
-------------------
   
* Build a simplistic application that adds and removes numbers and characters to a list

   * We'll use a variant record to store elements that are integers, floating point, or characters
   * The main program will add some elements to the list, remove some, and then print the list

* The first part of the lab will use :ada:`GNAT.Debug_Pools` to see how everything works "normally"

* The second part of the lab will use :ada:`System.Storage_Pools` to implement our own storage management

.. note::

  The :ada:`Database` package is (mostly) implemented in the prompt. You just need to figure
  out how to use the debug/storage pool

------------------
GNAT.Debug_Pools
------------------

* Use :ada:`GNAT.Debug_Pools` for storage management for the access type

* Modify :ada:`main` to print out debug information after very add/remove call

------------------------------------
GNAT.Debug_Pools Memory Management
------------------------------------

.. container:: source_include 146_storage_pools/lab/storage_pools/answer/memory_mgmt.ads-debug :code:Ada

.. note::

  If we write it this way, then the name of the storage pool for :ada:`Record_Access_T` is the same
  whether we use :ada:`Debug_Pools` or :ada:`Storage_Pools`

-------------------------------
GNAT.Debug_Pools Main Program
-------------------------------

.. container:: source_include 146_storage_pools/lab/storage_pools/answer/main.adb-debug :code:Ada

-----------------------------
Database Body Modifications
-----------------------------

.. container:: source_include 146_storage_pools/lab/storage_pools/answer/database.adb :code:Ada :number-lines:6 :start-after:database_begin :end-before:database_end

----------------------
System.Storage_Pools
----------------------

* Update the memory management package to use :ada:`System.Storage_Pools`

  * Print a warning when running low on memory
  * Raise :ada:`Storage_Error` when out of memory
  * Make sure your memory block is small enough you can see those two issues!

* You need to implement the three abstract subprograms

  * :ada:`Allocate` should keep track of how much memory is used and how close to full we are
  * :ada:`Deallocate` can be null (we're memory safe if we never release memory!)
  * :ada:`Storage_Size` needs to be implemented, but it's more for the runtime so we don't
    need to worry about it for our simple example.

-----------------------------------------------
System.Storage_Pools Memory Management (Spec)
-----------------------------------------------

.. container:: source_include 146_storage_pools/lab/storage_pools/answer/memory_mgmt.ads :code:Ada :number-lines:1

--------------------------------------------------
System.Storage_Pools Memory Management (Helpers)
--------------------------------------------------

.. container:: source_include 146_storage_pools/lab/storage_pools/answer/memory_mgmt.adb :code:Ada :number-lines:1 :start-after:memory_tracking_begin :end-before:memory_tracking_end

-----------------------------------------------
System.Storage_Pools Memory Management (APIs)
-----------------------------------------------

.. container:: source_include 146_storage_pools/lab/storage_pools/answer/memory_mgmt.adb :code:Ada :number-lines:53 :start-after:memory_apis_begin :end-before:memory_apis_end

-----------------------------------
System.Storage_Pools Main Program
-----------------------------------

.. container:: source_include 146_storage_pools/lab/storage_pools/answer/main.adb :code:Ada

