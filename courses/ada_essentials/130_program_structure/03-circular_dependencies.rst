=========================
Circular Dependencies
=========================

--------------------------------
Handling Circular Dependencies
--------------------------------

* Elaboration must be linear
* Package declarations cannot depend on each other

   - No linear order is possible

* Which package elaborates first?

.. image:: circular_dependencies.svg

--------------------------------------
Body-Level Cross Dependencies Are OK
--------------------------------------

* The bodies only depend on other packages' declarations
* The declarations are already elaborated by the time the bodies are elaborated

.. image:: mutual_dependencies.svg
   :width: 70%
   :align: center

--------------------------
Resulting Design Problem
--------------------------

* Good design dictates that conceptually distinct types appear in distinct package declarations

   - Separation of concerns
   - High level of *cohesion*

* Not possible if they depend on each other
* One solution is to combine them in one package, even though conceptually distinct

   - Poor software engineering
   - May be only choice, depending on language version

     - Best choice would be to implement both parts in a new package

-------------------------------------------
Circular Dependency in Package Declaration
-------------------------------------------

.. code:: Ada

   with Department;
   package Personnel is
     type Employee is private;
     procedure Assign
        (This : in Employee;
         To   : in out Department.Section);
         -- We need visiblity into Department package
   private
     type Employee is record
       Assigned_To : Department.Section;
     end record;
   end Personnel;

   with Personnel;
   package Department is
     type Section is private;
     procedure Choose_Manager
        (This : in out Section;
         Who  : in Personnel.Employee);
         -- We need visiblity into Personnel package
   [...]
   end Department;

------------------------
"limited with" Clauses
------------------------

* Solve the circular declaration dependency problem

   - Controlled circularity is now permitted

* Provide a :dfn:`limited view` of the specified package

   - Only type names are visible (including in nested packages)
   - Types are viewed as an :dfn:`incomplete type`

* Normal view

  .. code:: Ada

     package Personnel is
       type Employee is private;
       procedure Assign ...
     private
       type Employee is ...
     end Personnel;

* Implied limited view

  .. code:: Ada

     package Personnel is
       type Employee;
     end Personnel;

.. container:: speakernote

   Note that the names of nested packages are of course visible, otherwise we could not reference the names of types declared within them.

..
  language_version 2005

-----------------------------
What Is an Incomplete Type?
-----------------------------

* A type is :dfn:`incomplete` when its representation is completely unknown

   - Address can still be manipulated through an :ada:`access`
   - Can be a formal parameter or function result's type

      + Subprogram's completion needs the complete type
      + Actual parameter needs the complete type

.. code:: Ada

   type Incomplete_T;

* Can be declared in a **private** part of a package

  - And completed in its body
  - Used to implement opaque pointers

* Thus typically involves some advanced features

--------------------------------------
Legal Package Declaration Dependency
--------------------------------------

.. code:: Ada

   with Department;
   package Personnel is
     type Employee is private;
     procedure Assign (This : in Employee;
                        To : in out Department.Section);
   private
     type Employee is record
       Assigned_To : Department.Section;
     end record;
   end Personnel;

   limited with Personnel;
   package Department is
     type Section is private;
     procedure Choose_Manager (This : in out Section;
                                Who : in Personnel.Employee);
   private
     type Section is record
       Manager : access Personnel.Employee;
     end record;
   end Department;

..
  language_version 2005

----------------------------------------
Full "with" Clause on the Package Body
----------------------------------------

* Even though declaration has a :ada:`limited with` clause
* Typically necessary since body does the work

   - Dereferencing, etc.

* Usual semantics from then on

  .. code:: Ada

     limited with Personnel;
     package Department is
     ...
     end Department;

     with Personnel; -- normal view in body
     package body Department is
     ...
     end Department;

..
  language_version 2005

