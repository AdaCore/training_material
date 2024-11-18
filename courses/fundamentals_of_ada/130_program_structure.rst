*******************
Program Structure
*******************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==============
Introduction
==============

--------------
Introduction
--------------

* Moving to "bigger" issues of overall program composition
* How to compose programs out of program units
* How to control object lifetimes
* How to define subsystems

===================
Building a System
===================

-------------------
What Is a System?
-------------------

* Also called Application or Program or ...
* Collection of :dfn:`library units`

   - Which are a collection of packages or subprograms

----------------------
Library Units Review
----------------------

* Those units not nested within another program unit
* Candidates

   - Subprograms
   - Packages
   - Generic Units
   - Generic Instantiations
   - Renamings

* Dependencies between library units via :ada:`with` clauses

   - What happens when two units need to depend on each other?

=========================
Circular Dependencies
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

   with Department; --  Circular dependency
   package Personnel is
     type Employee is private;
     procedure Assign (This : in Employee;
                        To : in out Department.Section);
   private
     type Employee is record
       Assigned_To : Department.Section;
     end record;
   end Personnel;

   with Personnel; --  Circular dependency
   package Department is
     type Section is private;
     procedure Choose_Manager (This : in out Section;
                                Who : in Personnel.Employee);
   [...]
   end Department;

------------------------
`limited with` Clauses
------------------------

* Solve the cyclic declaration dependency problem

   - Controlled cycles are now permitted

* Provide a :dfn:`limited view` of the specified package

   - Only type names are visible (including in nested packages)
   - Types are viewed as :dfn:`incomplete types`

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

------------------------
Using Incomplete Types
------------------------

* A type is :dfn:`incomplete` when its representation is completely unknown

   - Address can still be manipulated through an :ada:`access`
   - Can be a formal parameter or function result's type

      + Subprogram's completion needs the complete type
      + Actual parameter needs the complete type

   - Can be a generic formal type parameters
   - If :ada:`tagged`, may also use `'Class`

.. code:: Ada

   type T;

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
Full `with` Clause on the Package Body
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

============================
Hierarchical Library Units
============================

----------------------------------
Problem: Packages Are Not Enough
----------------------------------

* Extensibility is a problem for private types

   - Provide excellent encapsulation and abstraction
   - But one has either complete visibility or essentially none
   - New functionality must be added to same package for sake of compile-time visibility to representation
   - Thus enhancements require editing/recompilation/retesting

* Should be something "bigger" than packages

   - Subsystems
   - Directly relating library items in one name-space

      + One big package has too many disadvantages

   - Avoiding name clashes among independently-developed code

--------------------------------------
Solution: Hierarchical Library Units
--------------------------------------

.. container:: columns

 .. container:: column

    * Address extensibility issue

       - Can extend packages with visibility to parent private part
       - Extensions do not require recompilation of parent unit
       - Visibility of parent's private part is protected

    * Directly support subsystems

       - Extensions all have the same ancestor *root* name

 .. container:: column

    .. image:: hierarchical_library_units.png

--------------------------
Programming by Extension
--------------------------

* :dfn:`Parent unit`

   .. code:: Ada

      package Complex is
        type Number is private;
        function "*" (Left, Right : Number) return Number;
        function "/" (Left, Right : Number) return Number;
        function "+" (Left, Right : Number) return Number;
        function "-" (Left, Right : Number) return Number;
      ...
      private
        type Number is record
          Real_Part, Imaginary_Part : Float;
        end record;
      end Complex;

* Extension created to work with parent unit

   .. code:: Ada

      package Complex.Utils is
        procedure Put (C : in Number);
        function As_String (C : Number) return String;
        ...
      end Complex.Utils;

-----------------------------------
Extension Can See Private Section
-----------------------------------

* With certain limitations

.. code:: Ada

   with Ada.Text_IO;
   package body Complex.Utils is
     procedure Put (C : in Number) is
     begin
       Ada.Text_IO.Put (As_String (C));
     end Put;
     function As_String (C : Number) return String is
     begin
       -- Real_Part and Imaginary_Part are
       -- visible to child's body
       return "(" & Float'Image (C.Real_Part) & ", " &
              Float'Image (C.Imaginary_Part) & ")";
     end As_String;
   ...
   end Complex.Utils;

--------------------
Subsystem Approach
--------------------

.. code:: Ada

   with Interfaces.C;
   package OS is -- Unix and/or POSIX
    type File_Descriptor is new Interfaces.C.int;
     ...
   end OS;

   package OS.Mem_Mgmt is
     ...
     procedure Dump (File               : File_Descriptor;
                      Requested_Location : System.Address;
                      Requested_Size     : Interfaces.C.Size_T);
     ...
   end OS.Mem_Mgmt;

   package OS.Files is
     ...
     function Open (Device : Interfaces.C.char_array;
                     Permission : Permissions := S_IRWXO)
                     return File_Descriptor;
     ...
   end OS.Files;

------------------------
Predefined Hierarchies
------------------------

* Standard library facilities are children of `Ada`

   - `Ada.Text_IO`
   - `Ada.Calendar`
   - `Ada.Command_Line`
   - `Ada.Exceptions`
   - et cetera

* Other root packages are also predefined

   - `Interfaces.C`
   - `Interfaces.Fortran`
   - `System.Storage_Pools`
   - `System.Storage_Elements`
   - et cetera

-------------------------
Hierarchical Visibility
-------------------------

.. container:: columns

 .. container:: column

    * Children can see ancestors' visible and private parts

       - All the way up to the root library unit

    * Siblings have no automatic visibility to each other
    * Visibility same as nested

       - As if child library units are nested within parents

          + All child units come after the root parent's specification
          + Grandchildren within children, great-grandchildren within ...

 .. container:: column

    .. image:: hierarchical_visibility.png

------------------------------------
Example of Visibility As If Nested
------------------------------------

.. code:: Ada

   package Complex is
     type Number is private;
     function "*" (Left, Right : Number) return Number;
     function "/" (Left, Right : Number) return Number;
     function "+" (Left, Right : Number) return Number;
     ...
   private
     type Number is record
       Real_Part : Float;
       Imaginary : Float;
     end record;
     package Utils is
       procedure Put (C : in Number);
       function As_String (C : Number) return String;
       ...
     end Utils;
   end Complex;

-------------------------------------------
`with` Clauses for Ancestors Are Implicit
-------------------------------------------

.. container:: columns

 .. container:: column

    * Because children can reference ancestors' private parts

       - Code is not in executable unless somewhere in the :ada:`with` clauses

    * Explicit clauses for ancestors are redundant but OK

 .. container:: column

    .. code:: Ada

       package Parent is
         ...
       private
         A : Integer := 10;
       end Parent;

       -- no "with" of parent needed
       package Parent.Child is
          ...
       private
         B : Integer := Parent.A;
         -- no dot-notation needed
         C : Integer := A;
       end Parent.Child;

-------------------------------------------
 `with` Clauses for Siblings Are Required
-------------------------------------------

* If references are intended

.. code:: Ada

   with A.Foo; --required
   package body A.Bar is
      ...
      -- 'Foo' is directly visible because of the
      -- implied nesting rule
      X : Foo.Typemark;
   end A.Bar;

------
Quiz
------

.. code:: Ada

   package Parent is
      Parent_Object : Integer;
   end Parent;

   package Parent.Sibling is
      Sibling_Object : Integer;
   end Parent.Sibling;

   package Parent.Child is
      Child_Object : Integer := ? ;
   end Parent.Child;

Which is (are) **NOT** legal initialization(s) of ``Child_Object``?

   A. :answermono:`Parent.Parent_Object + Parent.Sibling.Sibling_Object`
   B. :answermono:`Parent_Object + Sibling.Sibling_Object`
   C. :answermono:`Parent_Object + Sibling_Object`
   D. ``None of the above``

.. container:: animate

   A, B, and C are illegal because there is no reference to package
   :ada:`Parent.Sibling` (the reference to :ada:`Parent` is implied by the
   hierarchy). If :ada:`Parent.Child` had ":ada:`with Parent.Sibling;`", then
   A and B would be legal, but C would still be incorrect because there is
   no implied reference to a sibling.

===================
Visibility Limits
===================

-------------------------------------
Parents Do Not Know Their Children!
-------------------------------------

* Children grant themselves access to ancestors' private parts

   - May be created well after parent
   - Parent doesn't know if/when child packages will exist

* Alternatively, language *could have* been designed to grant access when declared

   - Like ``friend`` units in C++
   - But would have to be prescient!

      * Or else adding children requires modifying parent

   - Hence too restrictive

* Note: Parent body can reference children

   - Typical method of parsing out complex processes

----------------------------------------------
Correlation to C++ Class Visibility Controls
----------------------------------------------

.. container:: columns

 .. container:: column

   * Ada private part is visible to child units

      .. code:: Ada

         package P is
           A ...
         private
           B ...
         end P;
         package body P is
           C ...
         end P;

 .. container:: column

   * Thus private part is like the protected part in C++

      .. code:: C++

         class C {
         public:
           A ...
         protected:
           B ...
         private:
           C ...
         };

-------------------
Visibility Limits
-------------------

* Visibility to parent's private part is not open-ended

   - Only visible to private parts and bodies of children
   - As if only private part of child package is nested in parent

* Recall users can only reference exported declarations

   - Child public spec only has access to parent public spec

.. code:: Ada

   package Parent is
      ...
   private
      type Parent_T is ...
   end Parent;

   package Parent.Child is
     -- Parent_T is not visible here!
   private
     -- Parent_T is visible here
   end Parent.Child;

   package body Parent.Child is
    -- Parent_T is visible here
   end Parent.Child;

--------------------------------
Children Can Break Abstraction
--------------------------------

* Could **break** a parent's abstraction

  - Alter a parent package state
  - Alters an ADT object state

* Useful for reset, testing: fault injections...

.. code:: Ada

   package Stack is
      ...
   private
      Values : array (1 .. N) of Foo;
      Top : Natural range 0 .. N := 0;
   end Stack;

   package body Stack.Reset is
      procedure Reset is
      begin
        Top := 0;
      end Reset;
   end Stack.Reset;

--------------------------
Using Children for Debug
--------------------------

* Provide **accessors** to parent's private information
* eg internal metrics...

.. code:: Ada

   package P is
      ...
   private
     Internal_Counter : Integer := 0;
   end P;

.. code:: Ada

   package P.Child is
     function Count return Integer;
   end P.Child;

.. code:: Ada

   package body P.Child is
     function Count return Integer is
     begin
       return Internal_Counter;
     end Count;
   end P.Child;

------
Quiz
------

.. container:: latex_environment scriptsize

 .. container:: columns

  .. container:: column

   .. code:: Ada

      package P is
         Object_A : Integer;
      private
         Object_B : Integer;
         procedure Dummy_For_Body;
      end P;

      package body P is
         Object_C : Integer;
         procedure Dummy_For_Body is null;
      end P;

      package P.Child is
         function X return Integer;
      end P.Child;

  .. container:: column

   Which return statement would be legal in ``P.Child.X?``

      A.  :answermono:`return Object_A;`
      B.  :answermono:`return Object_B;`
      C.  ``return Object_C;``
      D.  None of the above

   .. container:: animate

      Explanations

      A. :ada:`Object_A` is in the public part of :ada:`P` - visible to any unit that :ada:`with`'s :ada:`P`
      B. :ada:`Object_B` is in the private part of :ada:`P` - visible in the private part or body of any descendant of :ada:`P`
      C. :ada:`Object_C` is in the body of :ada:`P`, so it is only visible in the body of :ada:`P`
      D. A and B are both valid completions

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
Solution 1: Move Type to Parent Package
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

* Via :ada:`private with` clause
* Syntax

   .. code:: Ada

      private with package_name {, package_name} ;

* Public declarations can then access private siblings

   - But only in their private part
   - Still prevents exporting contents of private unit

* The specified package need not be a private unit

   - But why bother otherwise

..
  language_version 2005

------------------------
`private with` Example
------------------------

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

..
  language_version 2005

-------------------------------------
Combining Private and Limited Withs
-------------------------------------

* Cyclic :ada:`limited with` clauses allowed
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

..
  language_version 2005

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

========
Lab
========

.. include:: labs/130_program_structure.lab.rst

=========
Summary
=========

---------
Summary
---------

* Hierarchical library units address important issues

   - Direct support for subsystems
   - Extension without recompilation
   - Separation of concerns with controlled sharing of visibility

* Parents should document assumptions for children

   - "These must always be in ascending order!"

* Children cannot misbehave unless imported ("with'ed")
* The writer of a child unit must be trusted

   - As much as if he or she were to modify the parent itself
