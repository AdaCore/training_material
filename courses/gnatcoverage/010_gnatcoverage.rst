**************
GNATcoverage
**************

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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

--------------
GNATcoverage
--------------

* Provides range of coverage analysis facilities with support for

  * Variety of measurement methods, coverage criteria and output formats
  * Consolidation features to report across multiple program executions

-------------------------
Coverage Data Gathering
-------------------------

* Coverage computed from two kinds of trace files

  * :dfn:`Binary traces`

    * Produced by instrumented execution environment with unmodifed version of program.
    * Traces contain low level information about executed blocks of machine instructions

  * :dfn:`Source traces`

    * Produced by modified version of program
    * Original source :dfn:`instrumented` to generate coverage data

* Note: This course will focus on *Source Traces* coverage

================
Coverage Types
================

--------------------
Statement Coverage
--------------------

* Each executed line gets flagged as :dfn:`covered`

  * Including object initialization

.. container:: columns

  .. container:: column

    .. container:: latex_environment scriptsize

      .. code:: Ada
        :number-lines: 3

        procedure Test_Statement
          (A, B, C :     Integer;
           Z       : out Integer) is
           Local : integer := A;
        begin
           Local := Local * 10;
           Local := Local + B * 100;
           if C > 0
           then
              Local := Local * 2;
              Z := Local + C * 1_000;
           end if;
        end Test_Statement;

  .. container:: column

      .. image:: gnatcoverage/statement_example.jpg

* Call :ada:`Test_Statement` with :ada:`(1, 2, Integer'Last)`

   * Congratulation: 100% Statement Coverage! But...

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

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada
        :number-lines: 17

        procedure Test_Decision
          (A, B, C :     Integer;
           Z       : out Integer) is
           Check : constant Boolean :=
             A > 0 and then (B**2 > 0 or else C**2 > 0);
        begin
           if Check
           then
              Z := A + B + C;
           else
              Z := A * B * C;
           end if;
        end Test_Decision;

  .. container:: column

      .. image:: gnatcoverage/decision_example.jpg

* Call :ada:`Test_Statement` with :ada:`(0, 0, 0)` and :ada:`(1, 1, Integer'Last)`

   * Congratulation: 100% Decision Coverage! But...

.. container:: animate

  * :ada:`Check` can be :ada:`True` or :ada:`False` without ever examining :ada:`C**2 > 0`

    * :ada:`False` when :ada:`A <= 0`
    * :ada:`True` when :ada:`A > 0` and :ada:`B >= 1`

--------------------------------------
Modified Condition/Decision Coverage
--------------------------------------

* Decision Coverage plus :dfn:`Unique-Cause` verification

  * Proof that, for each subcondition, changing just the subcondition can change the expression result

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

* So, to prove MC/DC for subcondition A, coverage results must show that **both** rows 2 and 6 have been executed

----------------------------------------------
Modified Condition/Decision Coverage Example
----------------------------------------------


.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada
        :number-lines: 30

        procedure Test_Mcdc
          (A, B, C :     Integer;
           Z       : out Integer) is
        begin
           if A > 0 and then B > 0
           then
              Z := A * B;
           end if;
           if A > 0 or else B > 0 or else C > 0
           then
              Z := Z + A + B + C;
           end if;
        end Test_Mcdc;

  .. container:: column

      .. image:: gnatcoverage/mcdc_example.jpg

* Call :ada:`Test_Statement` with :ada:`(1, 0, 0)`, :ada:`(0, 1, 0)`, and :ada:`(1, 1, 0)`

   * Better test results, but we need more tests
   * In general, if there are N subconditions, need N+1 sets of data to get complete MC/DC coverage

=======
Usage
=======

---------------------------
GNAT Studio Setup Options
---------------------------

:menu:`Edit` |rightarrow| :menu:`Project Properties` |rightarrow| :menu:`GNATcov`

.. image:: gnatcoverage/properties_dialog.jpg

* **Run** Switches (also used for **Instrument**)

  * *Coverage Level*

    * Type of coverage instrumentation to apply to executable

* **Coverage** Switches

  * *Coverage Level*

    * Type of coverage data to report

* **Extra Switches**

  * e.g. :command:`--units` to specify units of interest (rather than choosing project contents)

---------------------------------
Instrumentation via GNAT Studio
---------------------------------

.. image:: gnatcoverage/run_all_actions.jpg

* :menu:`Analyze` |rightarrow| :menu:`Coverage` |rightarrow| :menu:`GNATcoverage Source Traces` |rightarrow| :menu:`Run All Actions` |rightarrow| :filename:`main.adb`

* Same behavior as running the commands in sequence:

  * :menu:`Analyze` |rightarrow| :menu:`Coverage` |rightarrow| :menu:`GNATcoverage Source Traces` |rightarrow|

    * :menu:`Select prebuilt runtime`
    * :menu:`Instrumentation` |rightarrow| :filename:`main.adb`
    * :menu:`Build` |rightarrow| :filename:`main.adb`
    * :menu:`Run` |rightarrow| :filename:`main.adb`
    * :menu:`Generate Report` |rightarrow| :ada:`main`

----------------------------------------
GNAT Studio "Run All Actions" Commands
----------------------------------------

.. container:: latex_environment scriptsize

  ::

    gnatcov instrument -Pgnatcov/default.gpr --level stmt+mcdc
        --dump-trigger=atexit --dump-channel=bin-file --dump-filename-simple

    gprbuild -p -Pgnatcov/default.gpr --src-subdirs=gnatcov-instr
        --implicit-with=<gnatcov_rts_dir>/gnatcov_rts_full.gpr

    gnatcov/obj/main.exe

    gnatcov coverage -Pgnatcov/default.gpr -c stmt+mcdc --annotate=xcov+
        --output-dir=gnatcov/obj/ -T gnatcov/obj/main.exe.srctrace

---------------------------
"Instrument" Command Line
---------------------------

:command:`gnatcov instrument --level=<> <units-of-interest> [OPTIONS]`

* Arguments

  .. list-table::

    * - :command:`--level`

      - Strictest criterion instrumentation will allow assessing afterwards

    * - :command:`<units-of-interest>`

      - Set of units for which such assessment will be possible

* Common options

  .. list-table::

    * - :command:`--dump-trigger=[<trigger>]`

      - When coverage data should be injected into output

    * - 

      - *manual* - user requests coverage data (default)

    * - 

      - *atexit* - termination of application

    * - 

      - *main-end* - end of main subprogram (skipped if exception)

    * - 

      - *ravenscar-task-termination* - termination of main task

    * - :command:`--dump-channel=[<channel>]`

      - Mechanism used to output coverage data

    * -

      - *bin-file* - source trace file

    * -

      - *base64-stdout* - base64 encoded file

----------------------
"Cover" Command Line
----------------------

:command:`gnatcov coverage --level=<> <units-of-interest> [OPTIONS]`

.. container:: latex_environment tiny

  .. list-table::

    * - :command:`--annotate [FORM]` or :command:`-a [FORM]`

      - *form* can be asm, xcov, xcov+, html, html+, dhtml, report

    * - :command:`--output-dir [SUBDIR]`

      - Directory to store XCOV or HTML reports

    * - :command:`--trace [TRACE]` or :command:`-T [TRACE]`

      - Trace file name
