======================
General Access Types
======================

----------------------------
Using General Access Types
----------------------------

* To differentiate the access types, add the keyword :ada:`all`

  .. code:: Ada

    type Gen_Access_T is access all Integer;
    type Gen_Access_2 is access all Integer;
    Allocated : Gen_Access_T := new Integer'(123);

* Remember, we cannot compare pool-specific access types

  * Because each access type deals with its own memory pool

  .. code:: Ada

    type Access_T is access Integer;
    type Access_2 is access Integer;
    Acc1 : Access_T := new Integer;
    Acc2 : Access_2 := Access_2 (Acc1);  -- Compile error

* But general access types don't refer to a specific pool

  .. code:: Ada

    Alloc2 : Gen_Access_2 := Gen_Access_2 (Allocated);

-----------------------
Referencing the Stack
-----------------------

* By default, stack-allocated objects cannot be referenced

  * May be optimized into a register by the compiler

* :ada:`aliased` declares an object to be referenceable through an access value

  .. code:: Ada

     Object : aliased Integer;

* :ada:`'Access` attribute gives a reference to the object

  .. code:: Ada

     Access_Object : Gen_Access_T := Object'Access;

--------------------------
"Aliased" Object Example
--------------------------

.. code:: Ada

  with Ada.Text_IO; use Ada.Text_IO;
  procedure Example is
     -- two general access types
     -- (point to same type, so convertible)
     type Gen_Access_T is access all Integer;
     type Gen_Access_2 is access all Integer;
     -- Pointer to an Integer
     Acc_Object : Gen_Access_T;
     -- Integer on the stack
     Object     : aliased Integer := 100;
     -- Pointer to an Integer using different access type
     Other_Acc  : Gen_Access_2;
  begin
     Put_Line ("Before:" & Object'Image);
     -- Set pointer to location of Object
     Acc_Object     := Object'Access;
     -- Increment pointed-to Object
     Acc_Object.all := Acc_Object.all + 20;
     -- Convert pointer of one type to pointer of
     -- compatible type
     Other_Acc      := Gen_Access_2 (Acc_Object);
     -- Increment pointed-to Object
     Other_Acc.all  := Other_Acc.all + 3;
     Put_Line ("After:" & Object'Image);
  end Example;

:command:`Before: 100`

:command:`After: 123`

----------------------
"Aliased" Parameters
----------------------

* To ensure a subprogram parameter always has a valid memory address, define it as :ada:`aliased`

   * Ensures :ada:`'Access` (and :ada:`'Address`) are valid for the parameter

.. code:: Ada

   procedure Example (Param : aliased Integer);

   Object1 : aliased Integer;
   Object2 : Integer;

.. code:: Ada

   -- This is OK
   Example (Object1);

   -- Compile error: Object2 could be optimized away
   -- or stored in a register
   Example (Object2);

   -- Compile error: No address available for parameter
   Example (123);

-----------------------------------
Deallocating General Access Types
-----------------------------------

.. code:: Ada

   type Access_T is access all Integer;
   procedure Free is
      new Ada.Unchecked_Deallocation (Integer,
                                      Access_T);
   Object : aliased Integer := 0;
   Pointer : Access_T;

* :ada:`Ada.Unchecked_Deallocation` can be safely used on a general
  access type when it is used with :ada:`new`

  .. code:: Ada

    Pointer := new Integer'(1234);
    Free (Pointer);

* But behavior is **undefined** otherwise

  .. code:: Ada

    Pointer := Object'Access;
    Free (Pointer);

.. tip::

   If you need to create/free memory, best to use pool-specific
   access types

------
Quiz
------

.. code:: Ada

   type General_T is access all Integer;
   type Pool_T is access Integer;

   Aliased_Object : aliased Integer;
   Random_Object  : Integer;

   General_Ptr       : General_T;
   Pool_Specific_Ptr : Pool_T;

Which assignment(s) is (are) legal?

A. ``General_Ptr := Random_Object'Access;``
B. :answermono:`General_Ptr := Aliased_Object'Access;`
C. ``Pool_Specific_Ptr := Random_Object'Access;``
D. ``Pool_Specific_Ptr := Aliased_Object'Access;``

.. container:: animate

   :ada:`'Access` is only allowed for general access types
   (:ada:`General_T`). To use :ada:`'Access` on an object, the
   object **must** be :ada:`aliased`.
