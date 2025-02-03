**************
GNATcoverage
**************

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

--------------
GNATcoverage
--------------

* Provides range of coverage analysis facilities with support for

  * Variety of measurement methods, coverage criteria and output formats
  * Consolidation features to report across multiple program executions

* Sometimes, we only want coverage on certain units

  * Referred to as :dfn:`Units of Interest`
  * Typically new/modified units
  * Usually excludes any units used for testing

-------------------------
Coverage Data Gathering
-------------------------

* Coverage computed from two kinds of trace files

  * :dfn:`Binary traces`

    * Produced by instrumented execution environment with unmodifed version of program
    * Traces contain low level information about executed blocks of machine instructions

  * :dfn:`Source traces`

    * Produced by modified version of program
    * Original source :dfn:`instrumented` to generate coverage data

*Note: This course will focus on* **Source Traces** *coverage*

================
Coverage Types
================

--------------------
Statement Coverage
--------------------

* Each executed line gets flagged as :dfn:`covered`

  * Including object initialization

  .. image:: gnatdas/cover_statement.png
    :width: 75%

* Call :ada:`Test_Statement` with :ada:`(1, 2, Integer'Last, X)`

   * Congratulations: 100% Statement Coverage! But...

.. container:: animate

  * We have not tested :ada:`C <= 0`

    * Which is a problem because we don't assign :ada:`Z` in this case

  * We cannot tell if :ada:`Z := Local + C * 1_000;` raised an exception

    * Statement coverage shows we *reached* a line, not that it executed successfully

-------------------
Decision Coverage
-------------------

* Adds evaluation of boolean expressions to statement coverage

  * Not just branches - boolean objects as well

.. image:: gnatdas/cover_decision.png

* Call :ada:`Test_Decision` with :ada:`(0, 0, 0, X)` and :ada:`(1, 1, Integer'Last, X)`

   * Congratulations: 100% Decision Coverage! But...

.. container:: animate

  * :ada:`Check` can be :ada:`True` or :ada:`False` without ever examining :ada:`C**2 > 0`

    * :ada:`False` when :ada:`A <= 0`
    * :ada:`True` when :ada:`A > 0` and :ada:`B >= 1`

--------------------------------------
Modified Condition/Decision Coverage
--------------------------------------

* Decision Coverage plus *Unique-Cause* verification

  :dfn:`Independent Influence`
    For each subcondition, changing just the subcondition can change the expression result

* Simple example: :ada:`A and then (B or else C)`

.. list-table::
   :header-rows: 1
   :stub-columns: 3

  * - Row

    - A
    - B
    - C
    - Result

  * - 1)

    - F
    - F
    - F
    - *F*

  * - 2)

    - F
    - F
    - T
    - *F*

  * - 3)

    - F
    - T
    - F
    - *F*

  * - 4)

    - F
    - T
    - T
    - *F*

  * - 5)

    - T
    - F
    - F
    - *F*

  * - 6)

    - T
    - F
    - T
    - *T*

  * - 7)

    - T
    - T
    - F
    - *T*

  * - 8)

    - T
    - T
    - T
    - *T*


* Note that rows 2 and 6 show that, if :ada:`B` is False and :ada:`C` is True, changing :ada:`A` changes the result

  * Similarly for rows 5 and 7 for :ada:`B` and rows 5 and 6 for :ada:`C`
  * There can be multiple pairs of rows depending on the expression

* So, to prove MCDC for subcondition A, coverage results must show that **both** rows 2 and 6 have been executed

* Note that there are two types of MCDC coverage implementations

  * Unique Cause MCDC, where every subcondition must be shown to affect the outcome of the result
  * Masking MCDC, which allows conditions to be grouped, necessary with coupled conditions

----------------------------------------------
Modified Condition/Decision Coverage Example
----------------------------------------------

.. image:: gnatdas/cover_mcdc.png
  :width: 75%

* Call :ada:`Test_Mcdc` with :ada:`(1, 0, 0, X)`, :ada:`(0, 1, 0, X)`, and :ada:`(1, 1, 0, X)`

   * Better test results, but we need more tests
   * In general, if there are N subconditions, need N+1 sets of data to get complete MCDC coverage

   .. image:: gnatdas/cover_mcdc_expanded.png
