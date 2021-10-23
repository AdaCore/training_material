**************
Packages Lab
**************

=====
Lab
=====

--------------
Instructions
--------------

* Requirements

   - Create a program to add and remove integer values from a list

   - Program should allow user to do the following as many times as desired

      - Add an integer to the list
      - Remove all occurrences of an integer from the list
      - Print the values in the list

* Hints

   - Create (at least) three packages

      1. minimum/maximum integer values and maximum number of items in list
      2. User input (ensure value is in range)
      3. List ADT

   - :code:`with package_name;` gives access to :code:`package_name`

----------------------------------------------
Creating Packages in :toolname:`GNAT Studio`
----------------------------------------------

* Right-click on :filename:`src` node

* :menu:`New` :math:`\rightarrow` :menu:`Ada Package`

   - Fill in name of Ada package
   - Check the box if you want to create the package body in addition to the package spec

-----------------------------------
Packages Lab Solution - Constants
-----------------------------------

.. container:: source_include labs/answers/100_packages.txt :start-after:--Constants :end-before:--Constants :code:Ada

.. container:: speakernote

   Could use functions where the value is stored in the body - less recompilation if the value changes (but then they cannot be universal integers)

------------------------------
Packages Lab Solution - Input
------------------------------

.. container:: source_include labs/answers/100_packages.txt :start-after:--Input :end-before:--Input :code:Ada

-----------------------------------
Packages Lab Solution - List
-----------------------------------

.. container:: source_include labs/answers/100_packages.txt :start-after:--List :end-before:--List :code:Ada

------------------------------
Packages Lab Solution - Main
------------------------------

.. container:: source_include labs/answers/100_packages.txt :start-after:--Main :end-before:--Main :code:Ada
