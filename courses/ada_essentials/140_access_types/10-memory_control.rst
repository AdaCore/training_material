================
Memory Control
================

----------------------
System.Storage_Pools
----------------------

* Mechanism to allow coder control over allocation/deallocation process

  * Uses :ada:`Ada.Finalization.Limited_Controlled` to implement customized memory allocation and deallocation
  * Must be specified for each access type being controlled

    .. code:: Ada

      type Boring_Access_T is access Some_T;
      -- Storage Pools mechanism not used here
      type Important_Access_T is access Some_T;
      for Important_Access_T'storage_pool use My_Storage_Pool;
      -- Storage Pools mechanism used for Important_Access_T


-------------------------------------
System.Storage_Pools Spec (Partial)
-------------------------------------

.. code:: Ada

  with Ada.Finalization;
  with System.Storage_Elements;
  package System.Storage_Pools with Pure is
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

  private
    -- ...
  end System.Storage_Pools;

-----------------------------------
System.Storage_Pools Explanations
-----------------------------------

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

----------------------------------------
System.Storage_Pools Example (Partial)
----------------------------------------

.. code:: Ada

   subtype Index_T is Storage_Count range 1 .. 1_000;
   Memory_Block : aliased array (Index_T) of Interfaces.Unsigned_8;
   Memory_Used  : array (Index_T) of Boolean := (others => False);

   procedure Set_In_Use (Start  : Index_T;
                         Length : Storage_Count;
                         Used   : Boolean);

   function Find_Free_Block (Length : Storage_Count) return Index_T;

   procedure Allocate
     (Pool                     : in out Storage_Pool_T;
      Storage_Address          :    out System.Address;
      Size_In_Storage_Elements :        Storage_Count;
      Alignment                :        Storage_Count) is
      Index : Storage_Count := Find_Free_Block (Size_In_Storage_Elements);
   begin
      Storage_Address := Memory_Block (Index)'Address;
      Set_In_Use (Index, Size_In_Storage_Elements, True);
   end Allocate;

   procedure Deallocate
     (Pool                     : in out Storage_Pool_T;
      Storage_Address          :        System.Address;
      Size_In_Storage_Elements :        Storage_Count;
      Alignment                :        Storage_Count) is
   begin
      for I in Memory_Block'Range loop
         if Memory_Block (I)'Address = Storage_Address then
            Set_In_Use (I, Size_In_Storage_Elements, False);
         end if;
      end loop;
   end Deallocate;
