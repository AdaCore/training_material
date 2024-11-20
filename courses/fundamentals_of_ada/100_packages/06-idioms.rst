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

