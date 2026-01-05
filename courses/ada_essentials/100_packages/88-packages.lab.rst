========
Lab
========

--------------
Packages Lab
--------------

* Requirements

  - Create a program to build a list of simple mathematical equations
  - For each equation, print out if the result would be in range

    - Equations are two real numbers and a simple operation (+, -, *, /)

* Hint: create (at least) three packages

  - **Types** creates

    - Numeric type with a range
    - Equation record type
    - Mechanism to convert the record to a string

  - **Validation**

    - Verifies equation result would be in range

  - **List** contains

    - List of equations
    - Mechanism to retrieve each item in the list

  - Remember: :code:`with package_name;` gives access to :code:`package_name`

------------------------------------
Creating Packages in "GNAT Studio"
------------------------------------

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
