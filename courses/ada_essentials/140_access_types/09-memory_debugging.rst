==================
Memory Debugging
==================

------------------
GNAT.Debug_Pools
------------------

* Ada allows the coder to specify *where* the allocated memory comes from

  * Called :dfn:`Storage Pool`
  * Basically, connecting :ada:`new` and :ada:`Unchecked_Deallocation` with some other code
  * More details in the next section

  .. container:: latex_environment footnotesize

    .. code:: Ada

      type Linked_List_Ptr_T is access all Linked_List_T;
      for Linked_List_Ptr_T'storage_pool use Memory_Mgmt.Storage_Pool;

* GNAT uses this mechanism in the runtime package :ada:`GNAT.Debug_Pools` to track allocation/deallocation

  .. container:: latex_environment footnotesize

    .. code:: Ada

      with GNAT.Debug_Pools;
      package Memory_Mgmt is
        Storage_Pool : GNAT.Debug_Pools.Debug_Pool;
      end Memory_Mgmt;

---------------------------------
GNAT.Debug_Pools Spec (Partial)
---------------------------------

.. code:: Ada

  package GNAT.Debug_Pools is
  
     type Debug_Pool is new System.Checked_Pools.Checked_Pool with private;
  
     generic
        with procedure Put_Line (S : String) is <>;
        with procedure Put      (S : String) is <>;
     procedure Print_Info
       (Pool          : Debug_Pool;
        Cumulate      : Boolean := False;
        Display_Slots : Boolean := False;
        Display_Leaks : Boolean := False);
  
     procedure Print_Info_Stdout
       (Pool          : Debug_Pool;
        Cumulate      : Boolean := False;
        Display_Slots : Boolean := False;
        Display_Leaks : Boolean := False);
     --  Standard instantiation of Print_Info to print on standard_output.
  
     procedure Dump_Gnatmem (Pool : Debug_Pool; File_Name : String);
     --  Create an external file on the disk, which can be processed by gnatmem
     --  to display the location of memory leaks.
  
     procedure Print_Pool (A : System.Address);
     --  Given an address in memory, it will print on standard output the known
     --  information about this address
  
     function High_Water_Mark
       (Pool : Debug_Pool) return Byte_Count;
     --  Return the highest size of the memory allocated by the pool.
  
     function Current_Water_Mark
       (Pool : Debug_Pool) return Byte_Count;
     --  Return the size of the memory currently allocated by the pool.
  
  private
     -- ...
  end GNAT.Debug_Pools;

------------------------------
Displaying Debug Information
------------------------------

* Simple modifications to our linked list example

  * Create and use storage pool

    .. container:: latex_environment footnotesize

      `` ``

      .. code:: Ada

        with GNAT.Debug_Pools; -- Added
        procedure Simple is
           Storage_Pool : GNAT.Debug_Pools.Debug_Pool; -- Added
           type Some_Record_T;
           type Some_Record_Access_T is access all Some_Record_T;
           for Some_Record_Access_T'storage_pool
               use Storage_Pool; -- Added

      `` ``

  * Dump info after each :ada:`new`

    .. container:: latex_environment footnotesize

      `` ``

      .. code:: Ada

        Item                         := new Some_Record_T;
        GNAT.Debug_Pools.Print_Info_Stdout (Storage_Pool); -- Added
        Item.all := (Line, Head);

      `` ``

  * Dump info after :ada:`free`

    .. container:: latex_environment footnotesize

      `` ``

      .. code:: Ada

        Free (Item);
        GNAT.Debug_Pools.Print_Info_Stdout (Storage_Pool); -- Added
  
-------------------
Execution Results
-------------------

::

  Enter String: X
  Total allocated bytes :  24
  Total logically deallocated bytes :  0
  Total physically deallocated bytes :  0
  Current Water Mark:  24
  High Water Mark:  24

  Enter String: Y
  Total allocated bytes :  48
  Total logically deallocated bytes :  0
  Total physically deallocated bytes :  0
  Current Water Mark:  48
  High Water Mark:  48

  Enter String:
  List
    Y
    X
  Delete
  Total allocated bytes :  48
  Total logically deallocated bytes :  24
  Total physically deallocated bytes :  0
  Current Water Mark:  24
  High Water Mark:  48

