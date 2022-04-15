***********************
Naming Considerations
***********************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

-----------
Rationale
-----------

+ Project files assume source files have GNAT naming conventions

  Specification
    ``<unitname>[-<childunit>].ads``

  Body
    ``<unitname>[-<childunit>].adb``

+ Sometimes you want different conventions

  + Third-party libraries
  + Legacy code used different compiler

    + Changing filenames would make tracking changes harder

====================
Source File Naming
====================

----------------------------
Source File Naming Schemes
----------------------------

+ Allow arbitrary naming conventions

  + Other than GNAT default convention

+ May be applied to all source files in a project

  + Specified in a package named :ada:`Naming`

+ May be applied to specific files in a project

  + Individual attribute specifications

-------------------------------------
Foreign Default File Naming Example
-------------------------------------

.. code:: Ada

   project Legacy_Code is
      ...
      package Naming is
         for Casing use "lowercase";
         for Dot_Replacement use ".";
         for Spec_Suffix ("Ada") use ".spec";
         for Body_Suffix ("Ada") use ".body";
      end Naming;
      ...
   end Legacy_Code;

----------------------------------
GNAT Default File Naming Example
----------------------------------

.. code:: Ada

   project GNAT is
      ...
      package Naming is
         for Casing use "lowercase";
         for Dot_Replacement use "-";
         for Spec_Suffix ("Ada") use ".ads";
         for Body_Suffix ("Ada") use ".adb";
      end Naming;
      ...
   end GNAT;

------------------------------------
Individual (Arbitrary) File Naming
------------------------------------

+ Uses associative arrays to specify file names

  + Index is a string containing the unit name

    + Case insensitive

  + Value is a string containing the file name

    + Case sensitivity depends on host file system

+ Has distinct attributes for specs and bodies

   *for Spec ("<unit name>") use "<filename>";*

   :ada:`for Spec ("MyPack.MyChild") use "MMS1AF32.A";`

   :ada:`for Body ("MyPack.MyChild") use "MMS1AF32.B";`

=====
Lab
=====

.. include:: labs/030_naming_considerations.lab.rst
