==========================
Access Types
==========================

----------------------
Declaration Location
----------------------

* Can be at library level

   .. code:: Ada

      package P is
        type String_Access is access String;
      end P;

* Can be nested in a procedure

   .. code:: Ada

      package body P is
         procedure Proc is
            type String_Access is access String;
         begin
            ...
         end Proc;
      end P;

* Nesting adds non-trivial issues

   - Creates a nested pool with a nested accessibility
   - Don't do that unless you know what you are doing! (see later)

-------------
Null Values
-------------

* A pointer that does not point to any actual data has a :ada:`null` value
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

    - **Not** the type of the accessed object

   .. code:: Ada

         type A_T is access all T;
         procedure Proc (V : A_T); -- Primitive of A_T, not T

* Primitive of the type can be created with the :ada:`access` mode

    - **Anonymous** access type

   .. code:: Ada

         procedure Proc (V : access T); -- Primitive of T

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

   type R is record
     F1, F2 : Integer;
   end record;
   type A_Int is access Integer;
   type A_String is access all String;
   type A_R is access R;
   V_Int    : A_Int := new Integer;
   V_String : A_String := new String'("abc");
   V_R      : A_R := new R;

.. code:: Ada

   V_Int.all := 0;
   V_String.all := "cde";
   V_String (1) := 'z'; -- similar to V_String.all (1) := 'z';
   V_R.all := (0, 0);
   V_R.F1 := 1; -- similar to V_R.all.F1 := 1;

