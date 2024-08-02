===================
Memory Management
===================

--------------------
Simple Linked List
--------------------

* A linked list object typically consists of:

  * Content
  * "Indication" of next item in list

    * Fancier linked lists may reference previous item in list

* "Indication" is just a pointer to another linked list object

  * Therefore, self-referencing

* Ada does not allow a record to self-reference

------------------
Incomplete Types
------------------

* In Ada, an :dfn:`incomplete type` is just the word :ada:`type` followed by the type name

  * Optionally, the name may be followed by :ada:`(<>)` to indicate the full type may be unconstrained

* Ada allows access types to point to an incomplete type

  * Just about the only thing you *can* do with an incomplete type!

.. code:: Ada

   type Some_Record_T;
   type Some_Record_Access_T is access all Some_Record_T;

   type Unconstrained_Record_T (<>);
   type Unconstrained_Record_Access_T is access all Unconstrained_Record_T;

   type Some_Record_T is record
      Field : String (1 .. 10);
   end record;

   type Unconstrained_Record_T (Size : Index_T) is record
      Field : String (1 .. Size);
   end record;
   
--------------------
Linked List in Ada
--------------------

* Now that we have a pointer to the record type (by name), we can use it in the full definition of the record type

.. code:: Ada

   type Some_Record_T is record
      Field : String (1 .. 10);
      Next  : Some_Record_Access_T;
   end record;

   type Unconstrained_Record_T (Size : Index_T) is record
      Field    : String (1 .. Size);
      Next     : Unconstrained_Record_Access_T;
      Previous : Unconstrained_Record_Access_T;
   end record;

------------------------
Simplistic Linked List
------------------------

.. code:: Ada

  with Ada.Text_IO; use Ada.Text_IO;
  with Ada.Unchecked_Deallocation;
  procedure Simple is
     type Some_Record_T;
     type Some_Record_Access_T is access all Some_Record_T;
     type Some_Record_T is record
        Field : String (1 .. 10);
        Next  : Some_Record_Access_T;
     end record;
  
     Head : Some_Record_Access_T := null;
     Item : Some_Record_Access_T := null;
  
     Line : String (1 .. 10);
     Last : Natural;

   procedure Free is new Ada.Unchecked_Deallocation
     (Some_Record_T, Some_Record_Access_T);
  
  begin
  
     loop
        Put ("Enter String: ");
        Get_Line (Line, Last);
        exit when Last = 0;
        Line (Last + 1 .. Line'Last) := (others => ' ');
        Item                         := new Some_Record_T;
        Item.all                     := (Line, Head);
        Head                         := Item;
     end loop;
  
     Put_Line ("List");
     while Head /= null loop
        Put_Line ("  " & Head.Field);
        Head := Head.Next;
     end loop;

     Put_Line ("Delete");
     Free (Item);
     GNAT.Debug_Pools.Print_Info_Stdout (Storage_Pool);
  
  end Simple;

