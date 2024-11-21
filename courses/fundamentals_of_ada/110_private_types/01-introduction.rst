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

    .. image:: interface_vs_implementation.png
       :width: 70%

-------
Views
-------

* Specify legal manipulation for objects of a type

   - Types are characterized by permitted values and operations

* Some views are implicit in language

   - Mode :ada:`in` parameters have a view disallowing assignment

* Views may be explicitly specified

   - Disallowing access to representation
   - Disallowing assignment

* Purpose: control usage in accordance with design

   - Adherence to interface
   - Abstract Data Types

