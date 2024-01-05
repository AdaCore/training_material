===================
GNAT SAS Overview
===================

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

------------------------------
GNAT SAS In A Nutshell (1/2)
------------------------------

+ :toolname:`GNAT SAS` is a static analysis tool

  + Provides feedback **before** execution and test
  + Provides *as-built documentation* for code reviews

+ Helps identify and eliminate **vulnerabilities and bugs** early
+ Modular

  + Analyze entire project or a single file
  + Configure strictiness level

+ Review of analysis report

  + Filtering messages by category, severity, package...
  + Comparative analysis between runs
  + Shareable reviews database

------------------------------
GNAT SAS In A Nutshell (2/2)
------------------------------

+ Large Ada support

  + Usable with Ada 83, 95, 2005, 2012
  + No vendor lock-in, supports GNAT, Apex, GHS, ObjectAda, VADS

+ Bundled with a Coding Standards Checker and a Metrics Tool

  + :toolname:`GNATcheck` and :toolname:`GNATmetric`

+ Detects runtime and logic errors exhaustively

  + Initialization errors, run-time errors and assertion failures (16 rules)
  + Race condition errors: unprotected access to globals (3 rules)

+ Warns on dead or suspicious code (21 rules)

----------------------
GNAT SAS Integration
----------------------

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
+ Disable with ``--no-infer`` switch

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
