========
Lab
========

------------------
Access Types Lab
------------------
   
* Build an application that adds / removes items from a linked list

   * At any time, user should be able to

     * Add a new item into the "appropriate" location in the list
     * Remove an item without changing the position of any other item in the list
     * Print the list

* Required goals

  1. Implement **Add** functionality

    * For this step, "appropriate" means either end of the list (but consistent - always front or always back)

  2. Implement **Print** functionality
  3. Implement **Delete** functionality

-------------------------
Lab Solution - Database
-------------------------

.. container:: source_include 140_access_types/lab/access_types/answer/database.ads :code:Ada :number-lines:1

.. container:: source_include 140_access_types/lab/access_types/answer/database.adb :code:Ada :number-lines:1

-------------------------------------
Lab Solution - Database_List (Spec)
-------------------------------------

.. container:: source_include 140_access_types/lab/access_types/answer/database_list.ads :code:Ada :number-lines:1

-----------------------------------------------
Lab Solution - Database_List (Helper Objects)
-----------------------------------------------

.. container:: source_include 140_access_types/lab/access_types/answer/database_list.adb :start-after:helpers_begin :end-before:helpers_end :code:Ada :number-lines:1

-----------------------------------------------
Lab Solution - Database_List (Insert/Delete)
-----------------------------------------------

.. container:: source_include 140_access_types/lab/access_types/answer/database_list.adb :start-after:insert_and_delete_begin :end-before:insert_and_delete_begin :code:Ada :number-lines:35

---------------------
Lab Solution - Main
---------------------

.. container:: source_include 140_access_types/lab/access_types/answer/main.adb :code:Ada :number-lines:1
