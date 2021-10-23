*****************
Limited with
*****************

=========================
"limited with" Clauses
=========================

------------------------------
Handling Cyclic Dependencies
------------------------------

* Elaboration must be linear
* Package declarations cannot depend on each other

   - No linear order is possible

* Which package elaborates first?

.. image:: cyclic_dependencies.png
   :width: 50%
   :align: center

--------------------------------------
Body-Level Cross Dependencies Are OK
--------------------------------------

* The bodies only depend on other packages' declarations
* The declarations are already elaborated by the time the bodies are elaborated

.. image:: mutual_dependencies.png
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

----------------------------------------
Illegal Package Declaration Dependency
----------------------------------------

.. code:: Ada

   with Department;
   package Personnel is
     type Employee is private;
     procedure Assign ( This : in Employee;
                        To : in out Department.Section);
   private
     type Employee is record
       Assigned_To : Department.Section;
     end record;
   end Personnel;

   with Personnel;
   package Department is
     type Section is private;
     procedure Choose_Manager ( This : in out Section;
                                Who : in Personnel.Employee);
   private
     type Section is record
       Manager : Personnel.Employee;
     end record;
   end Department;

------------------------
`limited with` Clauses
------------------------

.. admonition:: Language Variant

   Ada 2005

* Solve the cyclic declaration dependency problem

   - Controlled cycles are now permitted

* Provide a "limited" view of the specified package

   - Only type names are visible (including in nested packages)
   - Types are viewed as *incomplete types*

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

------------------------
Using Incomplete Types
------------------------

* Anywhere that the compiler doesn't yet need to know how they are really represented

   - Access types designating them
   - Access parameters designating them
   - Anonymous access components designating them
   - As formal parameters and function results

      + As long as compiler knows them at the point of the call

   - As generic formal type parameters
   - As introductions of private types

* If :ada:`tagged`, may also use `'Class`
* Thus typically involves some advanced features

--------------------------------------
Legal Package Declaration Dependency
--------------------------------------

.. admonition:: Language Variant

   Ada 2005

.. code:: Ada

   limited with Department;
   package Personnel is
     type Employee is private;
     procedure Assign ( This : in Employee;
                        To : in out Department.Section);
   private
     type Employee is record
       Assigned_To : access Department.Section;
     end record;
   end Personnel;

   limited with Personnel;
   package Department is
     type Section is private;
     procedure Choose_Manager ( This : in out Section;
                                Who : in Personnel.Employee);
   private
     type Section is record
       Manager : access Personnel.Employee;
     end record;
   end Department;

----------------------------------------
Full `with` Clause On the Package Body
----------------------------------------

.. admonition:: Language Variant

   Ada 2005

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

===================
Private Children
===================

------------------
Private Children
------------------

* Intended as implementation artifacts
* Only available within subsystem

   - Rules prevent :ada:`with` clauses by clients
   - Thus cannot export anything outside subsystem
   - Thus have no parent visibility restrictions

      + Public part of child also has visibility to ancestors' private parts

.. code:: Ada

  private package Maze.Debug is
     procedure Dump_State;
     ...
  end Maze.Debug;

-------------------------------------------
Rules Preventing Private Child Visibility
-------------------------------------------

* Only available within immediate family

   - Rest of subsystem cannot import them

* Public unit declarations have import restrictions

   - To prevent re-exporting private information

* Public unit bodies have no import restrictions

   - Since can't re-export any imported info

* Private units can import anything

   - Declarations and bodies can import public and private units
   - Cannot be imported outside subsystem so no restrictions

--------------
Import Rules
--------------

* Only parent of private unit and its descendants can import a private child
* Public unit declarations import restrictions

   - Not allowed to have :ada:`with` clauses for private units

      + Exception explained in a moment

   - Precludes re-exporting private information

* Private units can import anything

   - Declarations and bodies can import private children

--------------------------------------
Some Public Children Are Trustworthy
--------------------------------------

* Would only use a private sibling's exports privately
* But rules disallow :ada:`with` clause

.. code:: Ada

   private package OS.UART is
    type Device is limited private;
    procedure Open (This : out Device; ...);
    ...
   end OS.UART;

   -- illegal - private child
   with OS.UART;
   package OS.Serial is
     type COM_Port is limited private;
     ...
   private
     type COM_Port is limited record
       -- but I only need it here!
       COM : OS.UART.Device;
     ...
     end record;
   end OS.Serial;

-----------------------------------------
Solution 1: Move Type To Parent Package
-----------------------------------------

.. code:: Ada

   package OS is
     ...
   private
     -- no longer an ADT!
     type Device is limited private;
   ...
   end OS;
   private package OS.UART is
     procedure Open (This : out Device;
      ...);
     ...
   end OS.UART;

.. code:: Ada

   package OS.Serial is
     type COM_Port is limited private;
     ...
   private
     type COM_Port is limited record
       COM : Device; -- now visible
       ...
     end record;
   end OS.Serial;

-------------------------------------------
Solution 2: Partially Import Private Unit
-------------------------------------------

.. admonition:: Language Variant

   Ada 2005

* Via :ada:`private with` clause
* Syntax

   .. code:: Ada

      private with package_name {, package_name} ;

* Public declarations can then access private siblings

   - But only in their private part
   - Still prevents exporting contents of private unit

* The specified package need not be a private unit

   - But why bother otherwise

------------------------
`private with` Example
------------------------

.. admonition:: Language Variant

   Ada 2005

.. code:: Ada

   private package OS.UART is
     type Device is limited private;
     procedure Open (This : out Device;
        ...);
     ...
   end OS.UART;

.. code:: Ada

   private with OS.UART;
   package OS.Serial is
     type COM_Port is limited private;
     ...
   private
     type COM_Port is limited record
       COM : OS.UART.Device;
       ...
     end record;
   end OS.Serial;

-------------------------------------
Combining Private and Limited Withs
-------------------------------------

.. admonition:: Language Variant

   Ada 2005

* Cyclic declaration dependencies allowed
* A public unit can :ada:`with` a private unit
* With-ed unit only visible in the private part

.. code:: Ada

   limited with Parent.Public_Child;
   private package Parent.Private_Child is
     type T is ...
   end Parent.Private_Child;

   limited private with Parent.Private_Child;
   package Parent.Public_Child is
     ...
   private
     X : access Parent.Private_Child.T;
   end Parent.Public_Child;

--------------------------------
Completely Hidden Declarations
--------------------------------

* Anything in a package body is completely hidden

   - Children have no access to package bodies

* Precludes extension using the entity

   - Must know that children will never need it

.. code:: Ada

   package body Skippy is
     X : Integer := 0;
     ...
   end Skippy;

-------------------
Child Subprograms
-------------------

* Child units can be subprograms

   - Recall syntax
   - Both public and private child subprograms

* Separate declaration required if private

   - Syntax doesn't allow :ada:`private` on subprogram bodies

* Only library packages can be parents

   - Only they have necessary scoping

.. code:: Ada

   private procedure Parent.Child;


