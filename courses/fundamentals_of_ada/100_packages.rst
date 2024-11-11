**********
Packages
**********

.. PRELUDE:: BEGIN

.. PRELUDE:: ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. PRELUDE:: SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. PRELUDE:: REQUIRES

.. PRELUDE:: PROVIDES

.. PRELUDE:: END

==============
Introduction
==============

----------
Packages
----------

* Enforce separation of client from implementation

   - In terms of compile-time visibility
   - For data
   - For type representation, when combined with :ada:`private` types

      + Abstract Data Types

* Provide basic namespace control
* Directly support software engineering principles

   - Especially in combination with :ada:`private` types
   - Modularity
   - Information Hiding (Encapsulation)
   - Abstraction
   - Separation of Concerns

-------------------------------
Basic Syntax and Nomenclature
-------------------------------

* Spec

   - Basic declarative items **only**
   - e.g. no subprogram bodies

      .. code:: Ada

            package name is
               {basic_declarative_item}
            end [name];

* Body

      .. code:: Ada

            package body name is
               declarative_part
            end [name];

-----------------------------------------
Separating Interface and Implementation
-----------------------------------------

* :dfn:`Implementation` and :dfn:`specification` are textually distinct from each other

   - Typically in separate files

* Clients can compile their code before body exists

   - All they need is the package specification
   - Clients have **no** visibility over the body
   - Full client/interface consistency is guaranteed

.. code:: Ada

   package Float_Stack is
     Max : constant := 100;
     procedure Push (X : in Float);
     procedure Pop (X : out Float);
   end Float_Stack;

---------------------------------
Uncontrolled Visibility Problem
---------------------------------

* Clients have too much access to representation

   - Data
   - Type representation

* Changes force clients to recode and retest
* Manual enforcement is not sufficient
* Why fixing bugs introduces new bugs!

==============
Declarations
==============

----------------------
Package Declarations
----------------------

* Required in all cases

   - Cannot have a package without the declaration

* Describe the client's interface

   - Declarations are exported to clients
   - Effectively the "pin-outs" for the black-box

* When changed, requires clients recompilation

   - The "pin-outs" have changed

.. code:: Ada

   package Float_Stack is
     Max : constant := 100;
     procedure Push (X : in Float);
     procedure Pop (X : out Float);
   end Float_Stack;

   package Data is
      Object : Integer;
   end Data;

---------------------------------
Compile-Time Visibility Control
---------------------------------

* Items in the declaration are visible to users

   .. code:: Ada

      package Some_Package is
        -- exported declarations of
        --   types, variables, subprograms ...
      end Some_Package;

* Items in the body are never externally visible

   - Compiler prevents external references

   .. code:: Ada

      package body Some_Package is
        -- hidden declarations of
        --   types, variables, subprograms ...
        -- implementations of exported subprograms etc.
      end Some_Package;

---------------------------------
Example of Exporting to Clients
---------------------------------

* Variables, types, exception, subprograms, etc.

   - The primary reason for separate subprogram declarations

.. code:: Ada

   package P is
      procedure This_Is_Exported;
   end P;

   package body P is
      procedure Not_Exported is
         ...
      procedure This_Is_Exported is
         ...
   end P;

============================
Referencing Other Packages
============================

----------------
 `with` Clause
----------------

* When package :ada:`Client` needs access to package :ada:`Server`, it uses a :ada:`with` clause

   - Specify the library units that :ada:`Client` depends upon
   - The "context" in which the unit is compiled
   - :ada:`Client`'s code gets **visibility** over :ada:`Server`'s specification


* Syntax (simplified)

   .. code:: Ada

      context_clause ::= { context_item }
      context_item ::= with_clause | use_clause
      with_clause ::= with library_unit_name
                      { , library_unit_name };

.. code:: Ada

   with Server; -- dependency
   procedure Client is

----------------------------
Referencing Exported Items
----------------------------

* Achieved via "dot notation"
* Package Specification

   .. code:: Ada

      package Float_Stack is
        procedure Push (X : in Float);
        procedure Pop (X : out Float);
      end Float_Stack;

* Package Reference

   .. code:: Ada

      with Float_Stack;
      procedure Test is
         X : Float;
      begin
         Float_Stack.Pop (X);
         Float_Stack.Push (12.0);
         ...

----------------------
`with` Clause Syntax
----------------------

* A library unit is a package or subprogram that is not nested within another unit

   - Typically in its own file(s)

     - e.g. for package :ada:`Test`, GNAT defaults to expect the spec in :filename:`test.ads` and body in :filename:`test.adb`)

* Only library units may appear in a :ada:`with` statement

   * Can be a package or a standalone subprogram

* Due to the :ada:`with` syntax, library units cannot be overloaded

   - If overloading allowed, which `P` would :ada:`with P;` refer to?

----------------
What To Import
----------------

* Need only name direct dependencies

   - Those actually referenced in the corresponding unit

* Will not cause compilation of referenced units

   - Unlike "include directives" of some languages

.. code:: Ada

   package A is
     type Something is ...
   end A;

   with A;
   package B is
     type Something is record
       Field : A.Something;
     end record;
   end B;

   with B; -- no "with" of A
   procedure Foo is
     X : B.Something;
   begin
     X.Field := ...

========
Bodies
========

----------------
Package Bodies
----------------

* Dependent on corresponding package specification

   - Obsolete if specification changed

* Clients need only to relink if body changed

   - Any code that would require editing would not have compiled in the first place

* Necessary for specifications that require a completion, for example:

   - Subprogram bodies
   - Task bodies
   - Incomplete types in :ada:`private` part
   - Others...

---------------------------
Bodies Are Never Optional
---------------------------

* Either required for a given spec or not allowed at all

   - Based on declarations in that spec

* A change from Ada 83
* A (nasty) justification example will be shown later

--------------------------------------
Example Spec That Cannot Have a Body
--------------------------------------

.. code:: Ada

   package Graphics_Primitives is
     type Coordinate is digits 12;
     type Device_Coordinates is record
       X, Y : Integer;
     end record;
     type Normalized_Coordinates is record
       X, Y : Coordinate range 0.0 .. 1.0;
     end record;
     type Offset is record
       X, Y : Coordinate range -1.0 .. 1.0;
     end record;
     -- nothing to implement, so no body allowed
   end Graphics_Primitives;

---------------------------------------
Example Spec Requiring a Package Body
---------------------------------------

.. code:: Ada

   package VT100 is
     subtype Rows is Integer range 1 .. 24;
     subtype Columns is Integer range 1 .. 80;
     type Position is record
       Row  : Rows := Rows'First;
       Col : Columns := Columns'First;
     end record;
      -- The following need to be defined in the body
     procedure Move_Cursor (To : in Position);
     procedure Home;
     procedure Clear_Screen;
     procedure Cursor_Up (Count : in Positive := 1);
   end VT100;

-----------------------
Required Body Example
-----------------------

.. code:: Ada

   package body VT100 is
     -- This function is not visible outside this package
     function Unsigned (Input : Integer) return String is
       Str : constant String := Integer'Image (Input);
     begin
       return Str (2 .. Str'Length);
     end Unsigned;
     procedure Move_Cursor (To : in Position) is
     begin
       Text_IO.Put (ASCII.Esc & 'I' &
                    Unsigned (To.Row) & ';' &
                    Unsigned (To.Col) & 'H');
     end Move_Cursor;
     procedure Home is
     begin
       Text_IO.Put (ASCII.Esc & "iH");
     end Home;
     procedure Cursor_Up (Count : in Positive := 1) is ...
       ...
   end VT100;

------
Quiz
------

.. include:: quiz/package_subprogram_completion/quiz.rst

==================
Executable Parts
==================

--------------------------
Optional Executable Part
--------------------------

.. code:: Ada

   package_body ::=
       package body name is
          declarative_part
       [ begin
          handled_sequence_of_statements ]
       end [ name ];

.. container:: speakernote

   Executable part is optional

---------------------------
Executable Part Semantics
---------------------------

* Executed only once, when package is elaborated
* Ideal when statements are required for initialization

   - Otherwise initial values in variable declarations would suffice

.. code:: Ada

   package body Random is
     Seed1, Seed2 : Integer;
     Call_Count : Natural := 0;
     procedure Initialize (Seed1 : out Integer;
                           Seed2 : out Integer) is ...
     function Number return Float is ...
   begin -- Random
     Initialize (Seed1, Seed2);
   end Random;

.. container:: speakernote

   Maybe initialization requires both values at once, hence two separate initializations (e.g., function calls) won't suffice, unlike CallCount.

------------------------------------------
Requiring/Rejecting Bodies Justification
------------------------------------------

.. container:: columns

 .. container:: column

    * Consider the alternative: an optional package body that becomes obsolete prior to building
    * Builder could silently choose not to include the package in executable

       - Package executable part might do critical initialization!

 .. container:: column

    .. code:: Ada

       package P is
         Data : array (L .. U) of
             Integer;
       end P;

       package body P is
         ...
       begin
         for K in Data'Range loop
           Data (K) := ...
         end loop;
       end P;

---------------------------------------
Forcing a Package Body to Be Required
---------------------------------------

.. container:: columns

 .. container:: column

    * Use :ada:`pragma Elaborate_Body`

       - Says to elaborate body immediately after spec
       - Hence there must be a body!

    * Additional pragmas we will examine later

 .. container:: column

    .. code:: Ada

       package P is
         pragma Elaborate_Body;
         Data : array (L .. U) of
             Integer;
       end P;

       package body P is
         ...
       begin
         for K in Data'Range loop
           Data (K) := ...
         end loop;
       end P;

========
Idioms
========

----------------------------------
Named Collection of Declarations
----------------------------------

* Exports:

   - Objects (constants and variables)
   - Types
   - Exceptions

* Does not export operations

.. code:: Ada

   package Physical_Constants is
     Polar_Radius_in_feet	: constant := 20_856_010.51;
     Equatorial_Radius_in_feet : constant := 20_926_469.20;
     Earth_Diameter_in_feet : constant := 2.0 *
        ((Polar_Radius_in_feet + Equatorial_Radius_in_feet)/2.0);
     Sea_Level_Air_Density : constant := 0.00239; --slugs/foot**3
     Altitude_Of_Tropopause_in_feet : constant := 36089.0;
     Tropopause_Temperature_in_celsius : constant := -56.5;
   end Physical_Constants;

--------------------------------------
Named Collection of Declarations (2)
--------------------------------------

* Effectively application global data

.. code:: Ada

   package Equations_of_Motion is
     Longitudinal_Velocity : Float := 0.0;
     Longitudinal_Acceleration : Float := 0.0;
     Lateral_Velocity  : Float := 0.0;
     Lateral_Acceleration : Float := 0.0;
     Vertical_Velocity : Float := 0.0;
     Vertical_Acceleration : Float := 0.0;
     Pitch_Attitude : Float := 0.0;
     Pitch_Rate : Float := 0.0;
     Pitch_Acceleration : Float := 0.0;
   end Equations_of_Motion;

--------------------------------
Group of Related Program Units
--------------------------------

* Exports:

   - Objects
   - Types
   - Values
   - Operations

* Users have full access to type representations

   - This visibility may be necessary

.. code:: Ada

   package Linear_Algebra is
     type Vector is array (Positive range <>) of Float;
     function "+" (L,R : Vector) return Vector;
     function "*" (L,R : Vector) return Vector;
     ...
   end Linear_Algebra;

--------------------------------------
Uncontrolled Data Visibility Problem
--------------------------------------

.. container:: columns

 .. container:: column

    * Effects of changes are potentially pervasive so one must understand everything before changing anything

 .. container:: column

    .. image:: subprograms_accessing_global.png

-------------------------
Packages and "Lifetime"
-------------------------

* Like a subprogram, objects declared directly in a package exist while the package is "in scope"

   * Whether the object is in the package spec or body

* Packages defined at the library level (not inside a subprogram) are always "in scope"

  * Including packages nested inside a package

* So package objects are considered "global data"

  * Putting variables in the spec exposes them to clients

    * Usually - in another module we talk about data hiding in the spec

  * Variables in the body can only be accessed from within the package body

--------------------------------------------
Controlling Data Visibility Using Packages
--------------------------------------------

* Divides global data into separate package bodies
* Visible only to procedures and functions declared in those same packages

   - Clients can only call these visible routines

* Global change effects are much less likely

   - Direct breakage is impossible

|

.. image:: packages_hiding_global_data.png
   :width: 85%

------------------------
Abstract Data Machines
------------------------

* Exports:

   - Operations
   - State information queries (optional)

* No direct user access to data

.. code:: Ada

   package Float_Stack is
     Max : constant := 100;
     procedure Push (X : in Float);
     procedure Pop (X : out Float);
   end Float_Stack;

   package body Float_Stack is
     type Contents is array (1 .. Max) of Float;
     Values : Contents;
     Top : Integer range 0 .. Max := 0;
     procedure Push (X : in Float) is ...
     procedure Pop (X : out Float) is ...
   end Float_Stack;

--------------------------------------------
Controlling Type Representation Visibility
--------------------------------------------

* In other words, support for Abstract Data Types

   - No operations visible to clients based on representation

* The fundamental concept for Ada
* Requires :ada:`private` types discussed in coming section...

========
Lab
========

.. include:: labs/100_packages.lab.rst

=========
Summary
=========

---------
Summary
---------

* Emphasizes separations of concerns
* Solves the global visibility problem

   - Only those items in the specification are exported

* Enforces software engineering principles

   - Information hiding
   - Abstraction

* Implementation can't be corrupted by clients

   - Compiler won't let clients compile references to internals

* Bugs must be in the implementation, not clients

   - Only body implementation code has to be understood
