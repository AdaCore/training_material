======================
Accessibility Checks
======================

--------------------------------------------
Introduction to Accessibility Checks (1/2)
--------------------------------------------

* The :dfn:`depth` of an object depends on its nesting within declarative scopes

   .. code:: Ada

      package body P is
         --  Library level, depth 0
         O0 : aliased Integer;
         procedure Proc is
            --  Library level subprogram, depth 1
            type Acc1 is access all Integer;
            procedure Nested is
               -- Nested subprogram, enclosing + 1, here 2
               O2 : aliased Integer;

* Objects can be referenced by access **types** that are at **same depth or deeper**

    - An **access scope** must be |le| the object scope

* :ada:`type Acc1` (depth 1) can access :ada:`O0` (depth 0) but not `O2` (depth 2)
* The compiler checks it statically

   - Removing checks is a workaround!

* Note: Subprogram library units are at **depth 1** and not 0

--------------------------------------------
Introduction to Accessibility Checks (2/2)
--------------------------------------------

.. code:: Ada

   package body P is
      type T0 is access all Integer;
      A0 : T0;
      V0 : aliased Integer;
      procedure Proc is
         type T1 is access all Integer;
         A1 : T1;
         V1 : aliased Integer;
      begin
         A0 := V0'Access;
         A0 := V1'Access; -- illegal
         A0 := V1'Unchecked_Access;
         A1 := V0'Access;
         A1 := V1'Access;
         A1 := T1 (A0);
         A1 := new Integer;
         A0 := T0 (A1); -- illegal
     end Proc;
   end P;

* To avoid having to face these issues, avoid nested access types

-------------------------------------
Getting Around Accessibility Checks
-------------------------------------

* Sometimes it is OK to use unsafe accesses to data
* :ada:`'Unchecked_Access` allows access to a variable of an incompatible accessibility level
* Beware of potential problems!

   .. code:: Ada

      type Acc is access all Integer;
      G : Acc;
      procedure P is
         V : aliased Integer;
      begin
         G := V'Unchecked_Access;
         ...
         Do_Something (G.all);
         G := null; -- This is "reasonable"
      end P;

.. container:: speakernote

   Not the best way to write code

-------------------------------------------
Using Access Types For Recursive Structures
-------------------------------------------

* It is not possible to declare recursive structure
* But there can be an access to the enclosing type

.. code:: Ada

   type Cell; -- partial declaration
   type Cell_Access is access all Cell;
   type Cell is record -- full declaration
      Next       : Cell_Access;
      Some_Value : Integer;
   end record;

------
Quiz
------

.. code:: Ada

   type Global_Access_T is access all Integer;
   Global_Pointer : Global_Access_T;
   Global_Object  : aliased Integer;
   procedure Proc_Access is
      type Local_Access_T is access all Integer;
      Local_Pointer : Local_Access_T;
      Local_Object  : aliased Integer;
   begin

Which assignment is **not** legal?

A. ``Global_Pointer := Global_Object'Access;``
B. :answermono:`Global_Pointer := Local_Object'Access;`
C. ``Local_Pointer  := Global_Object'Access;``
D. ``Local_Pointer  := Local_Object'Access;``

.. container:: animate

   Explanations

   A. Pointer type has same depth as object
   B. Pointer type is not allowed to have higher level than pointed-to object
   C. Pointer type has lower depth than pointed-to object
   D. Pointer type has same depth as object

