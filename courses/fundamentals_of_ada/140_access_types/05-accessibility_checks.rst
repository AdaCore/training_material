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

* Issues with nesting

.. include:: ../examples/140_access_types/nesting_issues/src/p.adb
    :code: Ada

* To avoid having to face these issues, avoid nested access types

------------------------------
Dynamic Accessibility Checks
------------------------------

* Following the same rules

    - Performed dynamically by the runtime

* Lots of possible cases

    - New compiler versions may detect more cases
    - Using access always requires proper debugging and reviewing

.. include:: ../examples/140_access_types/dynamic_accessibility_check/src/main.adb
    :code: Ada

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
Using Access Types for Recursive Structures
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
   Global_Access  : Global_Access_T;
   Global_Object  : aliased Integer;
   procedure Proc_Access is
      type Local_Access_T is access all Integer;
      Local_Access  : Local_Access_T;
      Local_Object  : aliased Integer;
   begin

Which assignment(s) is (are) legal?

A. :answermono:`Global_Access := Global_Object'Access;`
B. ``Global_Access := Local_Object'Access;``
C. :answermono:`Local_Access  := Global_Object'Access;`
D. :answermono:`Local_Access  := Local_Object'Access;`

.. container:: animate

   Explanations

   A. Access type has same depth as object
   B. Access type is not allowed to have higher level than accessed object
   C. Access type has lower depth than accessed object
   D. Access type has same depth as object

