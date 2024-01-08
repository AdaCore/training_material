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
  + Configure for speed or depth

+ Review of analysis report

  + Filtering messages by category, severity, package...
  + Comparative analysis between runs
  + Maintain historical comments

------------------------------
GNAT SAS In A Nutshell (2/2)
------------------------------

+ Large Ada support

  + Usable with Ada83 through Ada2022
  + No vendor lock-in

    + Supports GNAT, Apex, GHS, ObjectAda, VADS

+ Bundled with a Coding Standards Checker and a Metrics Tool

  + :toolname:`GNATcheck` and :toolname:`GNATmetric`

+ Detects runtime and logic errors exhaustively

  + Initialization errors, run-time errors and assertion failures
  + Race condition errors: unprotected access to globals

+ Warns on dead or suspicious code

----------------------
GNAT SAS Integration
----------------------

+ Output: textual, XML, CSV, HTML
+ Command-line tool (uses GNAT project files)
+ Interactive use in :toolname:`GNAT Studio` and :toolname:`GNATbench` IDEs
+ Integration with Jenkins (continuous builder)
+ Integration with :toolname:`SonarQube` (continuous inspection of code quality)

-----------------------------
Integrated Analysis Engines
-----------------------------

+ Inspector

  + Excels in detecting possibly failing run-time checks as well as wide range of logical errors
  + Determines preconditions on the inputs necessary to preclude run-time failures
  + Makes presumptions about return values of external subprograms
  + Identifies postconditions that characterize the range of outputs

+ Infer

  + https://fbinfer.com/
  + Specialized to Ada by AdaCore
  + Fast analysis with low false positive rate
  + Especially good in detecting problems occurring on certain program paths, such as null-pointer dereferences or memory leaks

+ GNAT Warnings

  + Provides warning issued by GNAT compiler frontend
  + Detects things like suspicious constructs and warnings when the compiler is sure an exception will be raised at run-time

+ GNATcheck

  + Tool used to check for suspicious code constructs and compliance with specified coding standard rules
  + Fully integrated with GNAT SAS

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
