==============
Introduction
==============

--------------
Introduction
--------------

* Why does fixing bugs introduce new ones?
* Control over visibility is a primary factor

   - Changes to an abstraction's internals shouldn't break **clients**
   - Including type representation

* Need tool-enforced rules to isolate dependencies

   - Between implementations of abstractions and their **clients**
   - In other words, "information hiding"

--------------------
Information Hiding
--------------------

.. container:: columns

  .. container:: column

    .. raw:: latex

       \vspace{5mm}

    Compare to integrated circuits

    * Hides implementation details from the end **client**
    * **Client** only sees the interface 

      * Not how it works underneath

  .. container:: column

    .. image:: interface_vs_implementation.svg
       :width: 50%

* Example - you can drive a car without knowing how the engine works:

  * **Interfaces:** steering wheel, pedals, etc
  * **Implementation:** engine, transmission, brake pads, etc

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

* **Purpose:** control usage in accordance with design

   - Adherence to interface
   - Abstract Data Types
