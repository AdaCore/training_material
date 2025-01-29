******************
Predefined Rules
******************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. role:: rust(code)
    :language: Rust

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==============
Introduction
==============

----------------------------
Accessing Predefined Rules
----------------------------

* Over 200 predefined rules within :toolname:`GNATcheck`

  * Can be found via command :command:`gnatcheck -h`

* Rules have been developed over many years for many uses

  * May have very specialized use cases
  * Some rules may contradict other rules

* Rules can be specified on the command line or via a file

  * Rule on the command line:

    .. container:: latex_environment tiny

      |
      | :command:`gnatcheck screen.adb -rules +Runnamed_exits`
      |

    Apply **unnamed_exits** rule (*unnamed exit statement*) in analysis of file :filename:`screen.adb`

  * Rules file:

    .. container:: latex_environment tiny

      |
      | :command:`gnatcheck screen.adb -rules -from=coding_standard.rules -ROTHERS_In_Aggregates`
      |

    Apply rules from :filename:`coding_standard.rules` except for rule **OTHERS_In_Aggregates**

-----------------------
Rules with Parameters
-----------------------

* Some rules have parameters

  ``Too_Many_Primitives``

    *Flag any tagged type declaration that has more than N user-defined primitive operations*

* To specify a parameter, the value comes immediately after the rule separated only by a colon (:)

  * Incorrect

      :command:`gnatcheck screen.adb -rules +RToo_Many_Primitives`

      ``gnatcheck: (too_many_primitives) parameter is required for +R``

  * Correct

      :command:`gnatcheck screen.adb -rules +RToo_Many_Primitives:3`

*Note: Some parameters are optional*

=============================
Predefined Rules Categories
=============================

---------------------
Style-Related Rules
---------------------

**Tasking Example**

  ``Volatile_Objects_Without_Address_Clauses``

    *Flag each volatile object without an address specification*

**Object Orientation Example**

  ``Visible_Components``

    *Flag type declarations located in visible part of a library package or a library generic package that can declare visible component*

**Portability Example**

  ``Forbidden_Pragmas``

    *Flag each use of the specified pragmas*

**Program Structure Example**

  ``Local_Packages``

    *Flag local packages declared in package and generic package spec*

**Programming Practice Example**

  ``Anonymous_Array``

    *Flag all anonymous array type definitions*

**Readability Example**

  ``Style_Checks``
  
    *Flags violations of the source code presentation and formatting rules according to the rule parameter(s) specified*

---------------------
Feature Usage Rules
---------------------

**Examples**

  ``Abort_Statements``

    *Flag abort statements*

  ``Numeric_Literals``

    *Flag each use of a numeric literal except for those matching certain requirements*

-----------------------
Metrics-Related Rules
-----------------------

**Examples**

  ``Metrics_Cyclomatic_Complexity``

    *Flag program units whose executable body exceeds the specified limit*

  ``Metrics_LSLOC``

    *Flag program units that exceed the specified limit*

-------------
SPARK Rules
-------------

**Examples**

  ``Overloaded_Operators``

    *Flag each function declaration that overloads an operator symbol*

  ``Slices``

    *Flag all uses of array slicing*
