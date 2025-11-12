==================
GNAT.Debug_Pools
==================

------------------
Memory Debugging
------------------

* Common use of :ada:`System.Storage_Pools` is for debugging memory usage

* GNAT uses this mechanism in the runtime package :ada:`GNAT.Debug_Pools` to track allocation/deallocation

  * Still need to create a memory pool object
  * But :ada:`GNAT.Debug_Pools` implements the allocate/deallocate/query routines

* Updated :ada:`Memory_Mgmt` package spec (from previous chapter)

  .. code:: Ada

    with GNAT.Debug_Pools;
    package Memory_Mgmt is
      Storage_Pool : GNAT.Debug_Pools.Debug_Pool;
    end Memory_Mgmt;

* :ada:`Integer_List` package doesn't need to change!

  .. code:: Ada

    type Linked_List_T;
    type Linked_List_Ptr_T is access all Linked_List_T;
    for Linked_List_Ptr_T'Storage_Pool use Memory_Mgmt.Storage_Pool;

-------------------------------
Useful GNAT.Debug_Pools API's
-------------------------------

.. code:: Ada

  generic
     with procedure Put_Line (S : String) is <>;
     with procedure Put      (S : String) is <>;
  procedure Print_Info
    (Pool          : Debug_Pool;
     Cumulate      : Boolean := False;
     Display_Slots : Boolean := False;
     Display_Leaks : Boolean := False);
  --  Pass in your own output routines
  
  procedure Print_Info_Stdout
    (Pool          : Debug_Pool;
     Cumulate      : Boolean := False;
     Display_Slots : Boolean := False;
     Display_Leaks : Boolean := False);
  --  Print_Info using standard output
  
  function High_Water_Mark
    (Pool : Debug_Pool) return Byte_Count;
  --  Return the highest size of the memory allocated by the pool
  
  function Current_Water_Mark
    (Pool : Debug_Pool) return Byte_Count;
  --  Return the size of the memory currently allocated by the pool
  
--------------------------------
Debug Information on Insertion
--------------------------------

* Modify our :ada:`Insert` implementation to dump the information

.. container:: source_include 146_storage_pools/examples/debug_pools/integer_list.adb code:Ada :start-after:insert_begin :end-before:insert_end

--------------------------------
Debug Information on Insertion
--------------------------------

* Create a main program to test our code and give us a final status

.. container:: source_include 146_storage_pools/examples/debug_pools/main.adb code:Ada

* Execution Results
  
  ::

    Total allocated bytes :  16
    Total logically deallocated bytes :  0
    Total physically deallocated bytes :  0
    Current Water Mark:  16
    High Water Mark:  16

    High:  16
    Current:  0
