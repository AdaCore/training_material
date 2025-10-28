===================
Access Type Usage
===================

-------------
Null Values
-------------

* Pointer that does not point to any actual data has a :ada:`null` value

  * Access types have a default value of :ada:`null`

* :ada:`null` can be used in assignments and comparisons

.. code:: Ada

   declare
      type Acc is access Integer;
      V : Acc;
   begin
      if V = null then
         --  will go here
      end if;
      V := new Integer'(0);
      V := null; -- semantically correct, but memory leak

---------------------------
Access Types and Primitives
---------------------------

* Subprogram using an access type are primitive of the **access type**

  * **Not** the type of the accessed object

.. container:: latex_environment footnotesize

  .. code:: Ada

    type Rec_T is null record;
    type Rec_Access_T is access Rec_T;
    procedure Proc1 (Param : Rec_T);         -- primitive of Rec_T
    procedure Proc2 (Param : Rec_Access_T);  -- primitive of Rec_Access_T

--------------------------
Dereferencing Access Types
--------------------------

* :ada:`.all` does the access dereference

   - Lets you access the object pointed to by the pointer

* :ada:`.all` is optional for

   - Access on a component of an array
   - Access on a component of a record

----------------------
Dereference Examples
----------------------

.. code:: Ada

   type Rec_T is record
     Field : Integer;
   end record;
   type Integer_Acc is access Integer;
   type String_Acc is access String;
   type Rec_Acc is access Rec_T;

   Integer_Ptr : Integer_Acc := new Integer;
   String_Ptr  : String_Acc  := new String'("abc");
   Rec_Ptr     : Rec_Acc  := new Rec_T;

.. code:: Ada

   -- Legal
   Integer_Ptr.all  := 0;
   String_Ptr.all   := "cde";
   String_Ptr(1)    := 'z';  -- or String_Ptr.all(1)
   Rec_Ptr.all      := (Field => 987);
   Rec_Ptr.Field    := 123;  -- or Rec_Ptr.all.Field

   -- Compile Errors
   Integer_Ptr := 0;
   String_Ptr  := "cde";
   Rec_Ptr     := (Field => 987);
