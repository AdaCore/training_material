========
Lab
========

---------------------------
Access Types In-Depth Lab
---------------------------
   
* Build an application that adds / removes items from a linked list

   * At any time, user should be able to

     * Add a new item into the "appropriate" location in the list
     * Remove an item without changing the position of any other item in the list
     * Print the list

* This is a multi-step lab! First priority should be understanding linked lists, then, if you have time, storage pools

* Required goals

  1. Implement **Add** functionality

    * For this step, "appropriate" means either end of the list (but consistent - always front or always back)

  2. Implement **Print** functionality
  3. Implement **Delete** functionality

--------------
Extra Credit
--------------

* Complete as many of these as you have time for

  1. Use :ada:`GNAT.Debug_Pools` to print out the status of your memory allocation/deallocation after every :ada:`new` and :ada:`deallocate`
  2. Modify **Add** so that "appropriate" means in a sorted order
  3. Implement storage pools where you write your own memory allocation/deallocation routines

    * Should still be able to print memory status

-------------------------
Lab Solution - Database
-------------------------

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/database.ads :code:Ada :number-lines:1

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/database.adb :code:Ada :number-lines:1

-------------------------------------
Lab Solution - Database_List (Spec)
-------------------------------------

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/database_list.ads :code:Ada :number-lines:1

-----------------------------------------------
Lab Solution - Database_List (Helper Objects)
-----------------------------------------------

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/database_list.adb :start-after:--Database_List_Helpers :end-before:--Database_List_Helpers :code:Ada :number-lines:1

-----------------------------------------------
Lab Solution - Database_List (Insert/Delete)
-----------------------------------------------

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/database_list.adb :start-after:--Database_List_Substance :end-before:--Database_List_Substance :code:Ada :number-lines:35

---------------------
Lab Solution - Main
---------------------

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/main.adb :code:Ada :number-lines:1

---------------------------------
Lab Solution - Simple_IO (Spec)
---------------------------------

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/simple_io.ads :code:Ada :number-lines:1

---------------------------------
Lab Solution - Simple_IO (Body)
---------------------------------

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/simple_io.adb :code:Ada :number-lines:1

------------------------------------------
Lab Solution - Memory_Mgmt (Debug Pools)
------------------------------------------

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/memory_mgmt.ads :code:Ada :number-lines:1

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/memory_mgmt.adb :code:Ada :number-lines:1

-------------------------------------------------
Lab Solution - Memory_Mgmt (Storage Pools Spec)
-------------------------------------------------

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/memory_mgmt.ads.storage :code:Ada :number-lines:1

------------------------------------------------
Lab Solution - Memory_Mgmt (Storage Pools 1/2)
------------------------------------------------

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/memory_mgmt.adb.storage :start-after:--Memory_Mgmt_Helpers :end-before:--Memory_Mgmt_Helpers :code:Ada :number-lines:1

------------------------------------------------
Lab Solution - Memory_Mgmt (Storage Pools 2/2)
------------------------------------------------

.. container:: source_include 140_access_types/lab/access_types-in_depth/answer/memory_mgmt.adb.storage :start-after:--Memory_Mgmt_Substance :end-before:--Memory_Mgmt_Substance :code:Ada :number-lines:49
