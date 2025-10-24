==============
Access Types
==============

--------------
Declarations
--------------

* Type is just a declaration of a pointer to another type

  .. code:: Ada

    type Access_T is access Integer;

* Unlike most scalar types, access types are not convertible

  * Even when pointing to same type

  .. code:: Ada
    :number-lines: 4

    type Access_Two_T is access Integer;
    Access_1 : Access_T;
    Access_2 : Access_Two_T := Access_Two_T (Access_1);

  :color-red:`example.adb:6:32: error: target type must be general access type`

-------
Usage
-------

* Use :ada:`new` to allocate space for an object

  .. code:: Ada

    Integer_Ptr : Access_T := new Integer;

* To allocate and initialize the pointed-to value, use a quantifier

  .. code:: Ada

    Integer_2_Ptr : Access_Two_T := new Integer'(123);

-------------
Null Values
-------------

* Pointer that does not point to any actual data has a :ada:`null` value

  * Access types have a default value of :ada:`null`

* :ada:`null` can be used in assignments and comparisons

.. code:: Ada

   declare
      type Acc is access all Integer;
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

.. code:: Ada

  type Record_T is null record;
  type Record_Access_T is access Record_T;
  procedure Proc1 (Param : Record_T);         -- primitive of Record_T
  procedure Proc2 (Param : Record_Access_T);  -- primitive of Record_Access_T

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

   type Record_T is record
     Field : Integer;
   end record;
   type Integer_Acc is access Integer;
   type String_Acc is access all String;
   type Record_Acc is access Record_T;

   Integer_Ptr : Integer_Acc := new Integer;
   String_Ptr  : String_Acc  := new String'("abc");
   Record_Ptr  : Record_Acc  := new Record_T;

.. code:: Ada

   -- Legal
   Integer_Ptr.all  := 0;
   String_Ptr.all   := "cde";
   String_Ptr(1)    := 'z';  -- or String_Ptr.all(1)
   Record_Ptr.all   := (Field => 987);
   Record_Ptr.Field := 123;  -- or Record_Ptr.all.Field

   -- Compile Errors
   Integer_Ptr := 0;
   String_Ptr  := "cde";
   Record_Ptr  := (Field => 987);
