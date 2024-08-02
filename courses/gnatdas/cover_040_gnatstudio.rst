*******************************
GNATcoverage From GNAT Studio
*******************************

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
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

------------------------------
Coverage Integrated with IDE
------------------------------

* Benefits of using :toolname:`GNAT Studio` for coverage processing

  * One click to instrument, build, run, analyze
  * Color-coded coverage view
  * Edit source code from coverage view

=======================================
Generating Coverage From GNAT Studio
=======================================

-----------------------
Setting Coverage Type
-----------------------

* On the command line, we used :command:`--level=stmt` for each step to specify coverage type

   * This allows us to generate a **Statement** report even if we've instrumented for **Statement** and **Decision**

* For :toolname:`GNAT Studio`, we simplify by using the same coverage type for all steps

   * :menu:`Edit` |rightarrow| :menu:`Project Properties`

      .. image:: gnatdas/cover_properties_dialog.png

--------------------
One-Click Coverage
--------------------

* Once the coverage type is set, you can use the menu or shortcut icon to instrument, build, and execute

    .. image:: gnatdas/cover_icon.png

   * :menu:`Analyze` |rightarrow| :menu:`Coverage` |rightarrow| :menu:`GNATcoverage Source Traces` |rightarrow| :menu:`Run All Actions`

    .. image:: gnatdas/cover_menu.png

------------------
Viewing Coverage
------------------

* Coverage report is displayed after execution

   .. image:: gnatdas/coverage_report.png

* Benefits from running coverage from :toolname:`GNAT Studio`

   * Coverage report is graphical and interactive
   * Annotated source code view

-----------------
Coverage Report
-----------------

.. container:: columns

   .. container:: column

      .. image:: gnatdas/coverage_report.png

      *Coverage report default view*

   .. container:: column

      .. image:: gnatdas/coverage_report_expanded.png

      *Coverage report with units expanded (shows subprograms)*

* Shows all source within project(s)

   * Numeric percentage of coverage
   * Graphical representation of coverage

* Click on column title to sort by 

   * Unit name (**Entities**)
   * Absolute coverage numbers (**Coverage**)
   * Relative coverage numbers (**%**)

-----------------------
Annotated Source Code
-----------------------

* Double-click on row in **Entities** column to show annotated code

   * Unit or subprogram

      .. image:: gnatdas/coverage_select_entity.png
         :width: 50%

* Selecting row in report vs unit in project view

   * Double-clicking row in report always brings up annotated view
   * Selecting unit in project view will

      * Open normal source view if unit not already displayed
      * Switch to displayed unit if tab already open (normal or annotated)

   * These are different *views* of the same file

      * So edits in one view automatically appear in other view
      * (That's why you don't have two tabs open!)

=============================
Updating Code with Coverage
=============================

-----------------------------
Typical Development Process
-----------------------------

* During development (typically called :dfn:`Code and Test`) you

   * Write your code
   * Run simple tests to make sure things don't blow up
   * Make sure all paths through your code are tested

* How does this process integrate with coverage instrumentation?

-------------------------------------
Updating Coverage When Code Changes
-------------------------------------

* Coverage executable is different from normal executable

   * Changes to code need to be added to coverage information
   * Only one executable is maintained

      * So if you build your normal executable, you need to rebuild the instrumented one to get coverage

* Simplest method of re-running to get updated coverage data

   * One-click method

      * ``Run GNATcoverage...`` icon
      * :menu:`Analyze` |rightarrow| :menu:`Coverage` |rightarrow| :menu:`GNATcoverage Source Traces` |rightarrow| :menu:`Run All Actions`

* Or manually do each step in the menu

   * Setup
   * Instrumentation
   * Build
   * Run
   * Generate Report

=====
Lab
=====

.. include:: labs/cover_040_gnatstudio.lab.rst
