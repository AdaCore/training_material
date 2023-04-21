**********************
:toolname:`CodePeer`
**********************

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

===================
CodePeer Overview
===================

-------------------
What Is CodePeer?
-------------------

+ Static analysis tool

  + Provides feedback **before** execution and test
  + Provides *as-built documentation* for code reviews

+ Modular

  + Analyze entire project or a single file
  + Configure strictiness level

+ Analyzes all Ada code

  + Usable with Ada 83, 95, 2005, 2012, 2022
  + No vendor lock-in - supports GNAT, Apex, GHS, ObjectAda, VADS

-----------------------
What Can CodePeer Do?
-----------------------

+ Helps identify and eliminate **vulnerabilities and bugs** early

+ Comprehensive analysis report

  + Filtering messages by category, severity, package...
  + Comparative analysis between runs
  + Shareable reviews database

+ Detects runtime and logic errors exhaustively

  + Initialization errors, run-time errors and assertion failures (16 rules)
  + Race condition errors: unprotected access to globals (3 rules)

+ Warns on dead or suspicious code (21 rules)

-------------------------------------
How Does :toolname:`CodePeer` Work?
-------------------------------------

+ :toolname:`CodePeer` computes the **possible** value

    + Of every **variable**
    + and every **expression**
    + at each **program point**

+ Starting with a **leaf** subprograms
+ Information is propagated up in the call-graph

    + Iterations to handle **recursion**

+ For each subprogram :ada:`Sub`

  + It generates a **precondition** guarding against :ada:`Sub` check failures
  + It issues **check/warning** messages for :ada:`Sub`
  + It generates a **postcondition** ensured by :ada:`Sub`
  + It uses the **generated contracts** to analyze calls to :ada:`Sub`

+ See *CodePeer By Example* for more details

   From :toolname:`GNAT Studio`

   :menu:`Help` :math:`\rightarrow` :menu:`Codepeer` :math:`\rightarrow` :menu:`Examples` :math:`\rightarrow` :menu:`Codepeer By Example`

----------------------------------
:toolname:`CodePeer` Integration
----------------------------------

+ Output: textual, XML, CSV, HTML
+ Command-line tool (uses GNAT project files)
+ Interactive use in :toolname:`GNAT Studio` and :toolname:`GNATbench` IDEs
+ Integration with Jenkins (continuous builder)
+ Integration with :toolname:`SonarQube` (continuous inspection of code quality)

-------------------------------
:toolname:`infer` Integration
-------------------------------

+ :toolname:`infer` for Ada on top of main analysis
+ Based on Facebook's :toolname:`infer` engine
+ Adds **lightweight** checks

-----------------------------
Typical Users And Use Cases
-----------------------------

+ Developers, during code-writing

  + **Fix** (local) problems before integration

+ Reviewers

  + **Annotate** code with analysis of potential problems
  + **Analyse** specific CWE issues

+ Project managers and quality engineers

  + **Track** reported vulnerabilities regularly
  + **Identify** new issues quickly

+ Software auditors

  + **Identify** overall vulnerabilities or hot spots
  + **Verify** compliance to quality standards

=================
Getting Started
=================

------------------------------
Command Line Interface (1/2)
------------------------------

:command:`codepeer -P <project> [-level <level>]` ...

``-P <gpr project-file>``
   Note: All files from the project (including subprojects) will be analyzed.

   Tip: if missing a project file, use the ``--simple-project`` switch

``--level 0|1|2|3|4|min|max``
   Specify the level of analysis performed:

  + 0/min (default): fast and light checkers
  + 1: fast and per subprogram analysis
  + 2: more accurate/slower, automatic partitioning per set of units
  + 3: more accurate and much slower
  + 4/max: global (exhaustive) analysis, no partitioning

  Warning: Level 4 may exceed memory capacity or take a very long time

------------------------------
Command Line Interface (2/2)
------------------------------

:command:`codepeer` ... :command:`[--output-msg[-only]] [--html[-only]]`

``--output-msg[-only] [--output-msg switches]``
   If specified, :toolname:`CodePeer` will output its results, in various
   formats.

   If ``--output-msg`` is given, :toolname:`CodePeer` will perform a new
   analysis, and output its results.

   If ``--output-msg-only`` is specified, no new
   analysis is performed, and the results from the previous run
   (of the same level) will be emitted.

   You can control this output by adding switches.

   e.g. ``--output-msg --csv --out report.csv`` to generate a CSV file

``--html, --html-only``
   Generate HTML output. If ``--html-only``, do not run any analysis
   but use the previous run.

---------------------------------------------------------
Running :toolname:`CodePeer` in :toolname:`GNAT Studio`
---------------------------------------------------------

.. image:: codepeer_from_gs.jpg

---------------------
Project File Set Up
---------------------

Let's explore sections 1.4, 1.5 and 1.6 of the User's Guide

+ `Link: Basic Project File Setup<http://docs.adacore.com/codepeer-docs/users_guide/_build/html/introduction.html#basic-project-file-setup>`_
+ `Link: Project File Setup<http://docs.adacore.com/codepeer-docs/users_guide/_build/html/introduction.html#project-file-setup>`_
+ `Link: Advanced Project File Setup<http://docs.adacore.com/codepeer-docs/users_guide/_build/html/introduction.html#advanced-project-file-setup>`_

---------------------------------------------------
:toolname:`CodePeer` Levels Depth and Constraints
---------------------------------------------------

+ The **higher** the level the **deeper** and **costlier** the analysis

.. container:: latex_environment

   .. list-table::
      :header-rows: 1

      * - *Level*

        - *Description*
        - *Code size*
        - *False positives*

      * - *0*

        - Infer only (default)
        - No limits
        - Lowest

      * - *1*

        - Subprograms
        - No limits
        - Few

      * - *2*

        - Groups of units
        - No limits
        - Some

      * - *3*

        - Semi-global
        - < 1 million SLOC
        - High

      * -

        - Automatic partitioning
        - CC < 40
        -

      * - *4*

        - Global and **exhaustive**
        - < 200 KSLOC
        - Highest

      * -

        - Flag all issues
        - CC < 20
        -

+ *SLOC* : Source lines of code
+ *CC* : Cyclomatic Complexity

--------------------------------------
:toolname:`CodePeer` Levels Use Case
--------------------------------------

+ The levels adapt to various **workflows** and **users**
+ The **lower** the level the **more frequently** it should be run

.. container:: latex_environment

   .. list-table::
      :header-rows: 1

      * - *Level*

        - *Condition*
        - *Workflow Step*
        - *Goal*

      * - *0*

        - None
        - Initial static analysis
        - Quick feedback

      * - *1*

        - Project set-up
        - After each commit
        - Sanity check

      * - *2*

        - Level 1 results clean
        - Integration, CI
        - Regular check

      * - *3*

        - Medium code base
        - Integration, Nightly
        - Manual review

      * -

        - Server run
        -
        - Baseline

      * - *4*

        - Small code base
        - Before production
        - Exhaustive review

      * -

        - Server run
        -
        -

--------------------------
"No False Positive" Mode
--------------------------

+ :command:`--level 0` or :command:`--messages min`
+ Suppresses messages **most likely** to be false positives
+ Allows programmers to **focus** initial work on likely problems
+ Can be combined with **any level** of analysis
+ :command:`--messages min` is default for levels 0, 1, and 2

----------------------------------------
Running :toolname:`CodePeer` regularly
----------------------------------------

+ Historical database (SQLite) stores all results **per level**

  + Can be stored in Configuration Management

+ :dfn:`Baseline` run

  + **Previous** run each new run is compared to
  + Differences of **messages** in :toolname:`CodePeer` report
  + Default: first run
  + :command:`--baseline` to change it

+ Typical use

  + **Nightly** :command:`--baseline` run on servers
  + **Daily** development compares to baseline

+ :command:`--cutoff` overrides it for a **single** run
+ Compare between two arbitrary runs with :command:`--cutoff` and :command:`--current`
