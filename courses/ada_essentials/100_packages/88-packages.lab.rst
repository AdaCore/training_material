========
Lab
========

--------------
Packages Lab
--------------

* Requirements

   - Create a program to build a list of simple mathematical equations and then
     print out if, for each equation in the list, the result would be in range

      - Equations are two floating point numbers and a simple operation (+, -, *, /)

* Hints

   - You should create the following packages

      - Types package that creates a numeric range, an equation record type, and
        a mechanism to convert the record to a string
      - Validation package to verify that the equation result would be in range
      - List package to store the list of equations, and a mechanism to retrieve
        each item in the list

   - Remember: :code:`with package_name;` gives access to :code:`package_name`

----------------------------------------------
Creating Packages in :toolname:`GNAT Studio`
----------------------------------------------

* Right-click on the source directory node

   - If you used a prompt, the directory is probably :file:`.`
   - If you used the wizard, the directory is probably :file:`src`

* :menu:`New` :math:`\rightarrow` :menu:`Ada Package`

   - Fill in name of Ada package
   - Check the box if you want to create the package body in addition to the package spec

-------------------------------
Packages Lab Solution - Types
-------------------------------

.. container:: source_include 100_packages/lab/packages/answer/types.ads :code:Ada :number-lines:1

.. container:: source_include 100_packages/lab/packages/answer/types.adb :code:Ada :number-lines:1

------------------------------------
Packages Lab Solution - Validation
------------------------------------

.. container:: source_include 100_packages/lab/packages/answer/validator.ads :code:Ada :number-lines:1

.. container:: source_include 100_packages/lab/packages/answer/validator.adb :code:Ada :number-lines:1

-----------------------------------
Packages Lab Solution - List
-----------------------------------

.. container:: source_include 100_packages/lab/packages/answer/list.ads :code:Ada :number-lines:1

.. container:: source_include 100_packages/lab/packages/answer/list.adb :code:Ada :number-lines:1

------------------------------
Packages Lab Solution - Main
------------------------------

.. container:: source_include 100_packages/lab/packages/answer/main.adb :code:Ada :number-lines:1
