========
Lab
========

----------------
Exceptions Lab
----------------

Numeric String Verifier

* Overview

  * Create an application that converts strings to numeric values

* Requirements

  * Create a package to define your numeric type
  * Define a primitive to convert a string to your numeric type
  * The primitive should raise your own exceptions

    * Out-of-range value: number does not fit in range ("1e999")
    * Illegal string: illegal character found ("xyz")
    * Bad format: sign character found after first character ("12-")

  * Main program should run multiple tests on the primitive

  * Exception handler should display

    * Full exception data for an out-of-range value
    * Information about the exception for an illegal string
    * Exception name for bad format

*If you have time: Add your own content to the exception*

-----------------------------------------
Exceptions Lab Solution - Numeric Types
-----------------------------------------

.. container:: source_include 190_exceptions/lab/exceptions/answer/numeric_types.ads :code:Ada :number-lines:1

.. container:: source_include 190_exceptions/lab/exceptions/answer/numeric_types.adb :code:Ada :number-lines:1

--------------------------------
Exceptions Lab Solution - Main
--------------------------------

.. container:: source_include 190_exceptions/lab/exceptions/answer/main.adb :code:Ada :number-lines:1
