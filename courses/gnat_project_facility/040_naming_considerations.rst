***********************
Naming Considerations
***********************

.. role:: ada(code)
   :language: ada

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

   project Rational is
      ...
      package Naming is
         for Casing use "lowercase";
         for Dot_Replacement use ".";
         for Spec_Suffix ("Ada")  use ".1.ada";
         for Body_Suffix ("Ada") use ".2.ada";
      end Naming;
      ...
   end Rational;

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

.. include:: labs/040_naming_considerations.lab.rst
