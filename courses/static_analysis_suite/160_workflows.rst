********************
CodePeer Workflows
********************

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

====================
CodePeer Use Cases
====================

--------------------
CodePeer Use Cases
--------------------

+ Analyzing code locally prior to **commit** (desktop)
+ **Nightly** runs on a server
+ Continuous runs on a server after each **push**
+ Any **combination** desktop/continuous/nightly run
+ **Per-project** software customization
+ **Compare** local changes with master
+ Multiple teams **reviewing** multiple subsystems
+ Use :toolname:`CodePeer` to generate a **security report**

----------------------------------------------
Analyzing Code Locally Prior To Commit (1/2)
----------------------------------------------

+ Each **developer** as a single user, on a **desktop** machine
+ After compilation, before testing.
+ Solution #1: File by File analysis

  + Use :toolname:`GNAT Studio` menu
  + :menu:`CodePeer` :math:`\rightarrow` :menu:`Analyze File`
  + On the files that were **modified**
  + Fastest, incremental

+ Solution #2

  + Run :command:`codepeer --level 1/2 --baseline`
  + Local **baseline** database used for comparison
  + Look at **added** messages only
  + More exhaustive
  + Uses past reviews (less false positives)

----------------------------------------------
Analyzing Code Locally Prior To Commit (2/2)
----------------------------------------------

+ If duration or number of messages is not good :math:`\rightarrow` refine the settings
+ For each new message:

   + If a real issue is found :math:`\rightarrow` Fix the code
   + If it is a false positive :math:`\rightarrow` Justify it with :ada:`pragma Annotate`

--------------
Nightly Runs
--------------

+ :toolname:`CodePeer` run daily on a dedicated server

    + With large resources
    + Exhaustive level (2 :math:`\rightarrow` 4)

+ Typically run nightly

    + Takes into account commits of the day
    + Provides results to users the next morning

+ Allows users to analyze and justify messages **manually**

    + Via the **web** interface
    + From :toolname:`GNAT Studio` by accessing the **database** remotely

+ At release, results can be committed under CM for **traceability** purposes

-----------------
Continuous Runs
-----------------

+ :toolname:`CodePeer` is run on a dedicated server

    + With large resources
    + Fast level (0 or 1)

+ No need to be exhaustive

    + Focus on **differences** from previous run

+ Continuous runs triggerred on repository events
+ Summary is sent to developers

    + Email
    + Web interface
      :command:`codepeer -Pprj --output-msg-only --show-added | grep "[added]"`

+ Developers then *fix the code*, or *justify the relevant messages*

  + via :ada:`pragma Annotate` in source code or via web interface.
  + or wait for the next nightly run to post a manual analysis via the HTML Output.

------------------------------
Combined Desktop/Nightly Run
------------------------------

+ **Fast** analysis of code changes done at each **developer's desk**
+ A longer and **more exhaustive** analysis is performed nightly
+ The developer can re-use the **nightly** database as a baseline for analysis
+ Database reviews **should** be stored in this database

    + No conflict with nightly runs
    + Updated every morning in the users' databases

---------------------------------
Combined Continuous/Nightly Run
---------------------------------

+ **Fast** analysis of code changes done at each **developer's desk**
+ A longer and **more exhaustive** analysis is performed nightly
+ Alternatively: a baseline run is performed nightly

    + Same level as continuous runs and :command:`--baseline`

+ Database reviews **should** be stored in this database

    + No conflict with nightly runs
    + Updated every morning in the continuous database

-----------------------------------------
Combined Desktop/Continuous/Nightly Run
-----------------------------------------

+ **Fast** analysis of code changes done at each **developer's desk**
+ A **more exhaustive** analysis of code changes done continuously **on a server**
+ A longer and **even more exhaustive** analysis is performed nightly
+ Database reviews **should** be stored in this database

    + No conflict with nightly runs
    + Updated every morning in the users' and continuous databases

--------------------------------------------
Software Customization Per Project/Mission
--------------------------------------------

+ A *core* version of the software gets branched out or instantiated

    + Modified on a **per-project/mission** basis

+ Objectives

  + Separate :toolname:`CodePeer` runs on **all** active branches
  + Database is used to **compare** runs on a **single** given branch

+ **Continuous solution**

  + Justify message via :ada:`pragma Annotate` **only**
  + Merge of justifications handled via **standard CM**
  + Advantage: Code is self-justified

+ **One shot solution**

  + **Version** the database alongside the code
  + At branch point database is **forked**
  + Database is maintained separately from there
  + Advantage: Can use database reviews

----------------------------------------------
Multiple Teams Analyzing Multiple Subsystems
----------------------------------------------

+ Large software system with **multiple** subsystems

    + Maintained by **different** teams

+ Perform a **separate** analysis for each subsystem

    + Using a separate workspace and database

+ Create one project file (.gpr) per subsystem
+ To resolve dependencies between subsystems, use :ada:`limited with`

   .. code:: Ada

      limited with "subsystem1";
      limited with "subsystem2";
      project Subsystem3 is
         ...
      end Subsystem3;

+ Run :toolname:`CodePeer` with:

   :command:`codepeer -Psubsystem1 --no-subprojects`

=======================
Comparing to Baseline
=======================

---------------
Baseline Runs
---------------

+ Analysis running with latest source version

  + On a server

+ Baseline run

  + **Reference** database

    + Is a *gold* reference

  + **All changes** are compared to it
  + **All reviews** should be pushed to it

+ Create a baseline run

  + :command:`codepeer --baseline`

--------------------------------------
Baseline With Continuous Integration
--------------------------------------

+ Developers pre-validate changes **locally** prior to commit

    + Then create a **separate** branch and commits to it

+ The continuous builder is **triggered**

  + Database is copied from the **Baseline** run
  + Setting are copied from the **Reference** run settings

+ Results are reviewed via a spreadsheet tool (e.g. Excel)
+ Reviews are imported into the :toolname:`CodePeer` database

  + Can use :command:`--show-added` to show only the **new** messages

  .. container:: latex_environment tiny

     :command:`codepeer -Pprj --output-msg --show-added | grep "[added]"`

============================
CodePeer for Certification
============================

------------------
CodePeer and CWE
------------------

+ MITRE's Common Weakness Enumeration (CWE)

    + **Common** vulnerabilities in **software** applications
    + Referenced in many government contracts and cyber-security **requirements**

+ :toolname:`CodePeer` is officially **CWE-compatible**

  https://cwe.mitre.org/compatible/questionnaires/43.html

+ :toolname:`CodePeer` findings are **mapped** to CWE identifiers

.. code:: Ada

  project Prj1 is
     ...
     package CodePeer is
        for CWE use "true";
     end CodePeer;
   end Prj1;

.. code:: ada

    -- assign.adb:1: (pre)- assign:(overflow check [CWE 190])
    -- Y /= 2_147_483_647

------------------------
CodePeer and DO178B/C
------------------------

+ :toolname:`CodePeer` **supports** DO-178B/C Avionics Standard
+ DO-178C Objective A-5.6 (activity 6.3.4.f):

  **Code Accuracy and Consistency** (emphasis added)

  The objective is to determine the correctness and consistency of the Source Code, including stack usage, memory usage, **fixed point arithmetic overflow and resolution**, **floating-point arithmetic**, resource contention and limitations, worst-case execution timing, exception handling, **use of uninitialized variables**, cache management, **unused variables**, and **data corruption due to task or interrupt conflicts**.

  The compiler (including its options), the linker (including its options), and some hardware features may have an impact on the worst-case execution timing and this impact should be assessed.

+ :toolname:`CodePeer` **reduces** the scope of manual review
+ See Booklet: `Link: AdaCore Technologies for DO-178C/ED-12C <https://www.adacore.com/books/do-178c-tech>`_

  + Authored by Frederic Pothon & Quentin Ochem

--------------------------------
CodePeer and CENELEC - EN50128
--------------------------------

+ :toolname:`CodePeer` **qualified** as a T2 tool for this CENELEC Rail Standard
+ :toolname:`CodePeer` supports:

  + D.4 Boundary Value Analysis
  + D.8 Control Flow Analysis
  + D.10 Data Flow Analysis
  + D.14 Defensive Programming
  + D.18 Equivalence Classes and Input Partition Testing
  + D.24 Failure Assertion Programming
  + D.32 Impact Analysis

+ :toolname:`CodePeer` is uniquely supportive of Walkthroughs and Design Reviews via its as-built documentation
+ See Booklet: `Link: AdaCore Technologies for CENELEC EN 50128:2011 <https://www.adacore.com/books/cenelec-en-50128-2011>`_

  + Authored by Jean-Louis Boulanger & Quentin Ochem

============================
Limitations and Heuristics
============================

----------------------
CodePeer Limitations
----------------------

+ Entire code base may not be available for inspection

  + Only specs have been written
  + Library interfaces
  + Code too large so it has been *partitioned* for inspection

------------------
Evaluation Order
------------------

+ :toolname:`CodePeer` assumes parameters evaluated right to left

  + Compiler allowed to evaluate in any order

+ Subprograms with side effects may behave differently than :toolname:`CodePeer` assumes

--------------
Presumptions
--------------

+ In partitioned analysis, :toolname:`CodePeer` makes assumptions about *unanalyzed subprogram calls*

  + Subprograms in a different partition than one being analyzed

+ Presumptions are listed in information block

+ Presumptions can be overly optimistic

  1. Analyzed subprogram calls unanalyzed subprogram to return pointer
  2. Analyzed subprogram dereferences return value without checking
  3. :toolname:`CodePeer` might assume subprogram cannot return null

+ Presumptions can be insufficient

  1. Analyzed subprogram calls unanalyzed subprogram to return pointer
  2. :toolname:`CodePeer` might assume subprogram **can** return null
  3. Unanalyzed subprogram actually **cannot** return null
  4. Analyzed subprogram dereferences pointer
  5. :toolname:`CodePeer` flags possible dereference of null pointer

---------------
Generic Units
---------------

+ :toolname:`CodePeer` does not analyze generics

  + Instead it analyzes instantiations

+ If no instantiations - no analysis

+ If multiple instantiations - may get same message for every instantiation


=========
Summary
=========

------------
References
------------

  + Online: https://www.adacore.com/documentation#CodePeer
  + In local install at share/doc/codepeer/users_guide (or tutorial)
  + From :toolname:`GNAT Studio` go to :menu:`Help` :math:`\rightarrow` :menu:`Codepeer` :math:`\rightarrow` :menu:`Codepeer User's Guide` (or :menu:`Codepeer Tutorial`)

+ :toolname:`CodePeer` website

  + http://www.adacore.com/static-analysis/codepeer
  + Videos, product pages, articles, challenges

+ Book chapter on :toolname:`CodePeer`

  + In Static Analysis of Software: The Abstract Interpretation, published by Wiley (2012)
