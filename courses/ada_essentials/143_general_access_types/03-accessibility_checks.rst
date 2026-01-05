======================
Accessibility Checks
======================

---------------------
Access Type Scoping
---------------------

* The :dfn:`depth` of an object depends on its nesting within declarative scopes

  .. code:: Ada

     package body P is
        -- Library level, depth 0
        Object_0 : aliased Integer;
        procedure Proc is
           -- Library level subprogram, depth 1
           type Acc1 is access all Integer;
           procedure Nested is
              -- Nested subprogram, enclosing + 1, here 2
              Object_2 : aliased Integer;

* Objects can be referenced by access **types** that are at **same depth or deeper**

    - An **access scope** must be |le| the object scope

* :ada:`type Acc1` (depth 1) can access :ada:`Object_0` (depth 0) but not :ada:`Object_2` (depth 2)
* The compiler checks it statically

   - Removing checks is a workaround!

* Note: Subprogram library units are at **depth 1** and not 0

-----------------------------
Access Type Scoping Example
-----------------------------

* Issues with nesting

.. container:: source_include 143_general_access_types/examples/accessibility_checks/nesting_example.adb :code:Ada :number-lines:1

:color-red:`nesting_example.adb:12:23: error: non-local pointer cannot point to local object`

:color-red:`nesting_example.adb:17:39: error: cannot convert local pointer to non-local access type`

* To avoid having to face these issues, avoid nested access types

------------------------------
Dynamic Accessibility Checks
------------------------------

* Following the same rules

  - Performed dynamically by the runtime

* Runtime error when scoping is invalid but compiler could not detect it

* Lots of possible cases

  - New compiler versions may detect more cases
  - Using access always requires proper debugging and reviewing

.. container:: source_include 143_general_access_types/examples/accessibility_checks/dynamic_accessibility.adb :code:Ada :number-lines:4 :start-after:snippet_begin :end-before:snippet_end

:color-red:`raised PROGRAM_ERROR : dynamic_accessibility.adb:12 accessibility check failed`

-------------------------------------
Getting Around Accessibility Checks
-------------------------------------

* Sometimes it is OK to use unsafe accesses to data
* :ada:`'Unchecked_Access` allows access to a variable of an incompatible accessibility level
* Beware of potential problems!

  .. code:: Ada

     type Gen_Access_T is access all Integer;
     Global_Acc : Gen_Access_T;
     procedure Example is
        Local : aliased Integer;
     begin
        Global_Acc := Local'Unchecked_Access;
        ...
        Do_Something (Global_Acc.all);
        Global_Acc := null; -- This is "reasonable"
     end Example;

.. container:: speakernote

   Not the best way to write code

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

