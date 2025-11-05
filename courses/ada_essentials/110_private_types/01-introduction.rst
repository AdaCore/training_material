==============
Introduction
==============

--------------
Introduction
--------------

* Why does fixing bugs introduce new ones?
* Control over visibility is a primary factor

   - Changes to an abstraction's internals shouldn't break users
   - Including type representation

* Need tool-enforced rules to isolate dependencies

   - Between implementations of abstractions and their users
   - In other words, "information hiding"

--------------------
Information Hiding
--------------------

.. container:: columns

 .. container:: column

    * A design technique in which implementation artifacts are made inaccessible to users
    * Based on control of visibility to those artifacts

       - A product of "encapsulation"
       - Language support provides rigor

    * Concept is "software integrated circuits"

 .. container:: column

    .. image:: interface_vs_implementation.svg
       :width: 70%

Example: You can drive a car without knowing anything about how the engine works

  * Interface - steering wheel, pedals, etc
  * Implementation - engine, drivetrain, brake pads, etc

-------
Views
-------

* Specify legal manipulation for objects of a type

  :ada:`type Integer_T is range 0 .. 100;`

    * Can use math operators, comparison operators, assignment, ...

  :ada:`type Enumerated_T is (Red, Yellow, Green);`

    - Can use comparison operators, assignment, ...

* Some views are implicit in language

  .. code:: Ada

    procedure Increment (Value  : in out Integer; 
                         Amount : in     Integer);

  - **Value** has all operations available
  - **Amount** is read-only

* Views may be explicitly specified

  .. code:: Ada

    Initial_Value : constant Float := 32.0;

  - **Initial_Value** cannot be assigned a new value

* Purpose: control usage in accordance with design

   - Adherence to interface
   - Abstract Data Types
