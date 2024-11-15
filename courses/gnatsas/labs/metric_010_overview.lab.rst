----------------------
GNATmetric Lab Setup
----------------------

* Copy the :filename:`metric_010_overview` folder from the course materials location

  * This folder contains a project file (:filename:`default.gpr`) and some Ada source files

* From a command prompt, type :command:`gnatmetric --help` to verify your path is set correctly

  * If not, add the appropriate :filename:`bin` directory to your path
  * Typically (for Windows), this is located in ``C:\GNATSAS\<version>\bin``

-----------------------------
GNATmetric Lab - GUI Part 1
-----------------------------

* Use :toolname:`GNAT Studio` to open the project :filename:`default.gpr`

* Open :filename:`line_metrics_example.adb`

* Perform metrics analysis to get all line metrics on this file

  * :menu:`Analyze` -> :menu:`Metrics` -> :menu:`Compute Metrics on Current File`
  * Select **All line metrics** and press :menu:`Execute`

.. container:: animate 1-

  * Question 1: How many lines in the file? In subprogram :ada:`Example`?

.. container:: animate 2-

  19 lines in the file, 8 lines in :ada:`Example`

.. container:: animate 3-

  * Question 2: Is there any information for the package spec?

.. container:: animate 4-

  No - :menu:`Current File` means actual file, not package

-----------------------------
GNATmetric Lab - GUI Part 2
-----------------------------

Perform metrics analysis to get all complexity metrics in the project

.. container:: animate 2-

  * :menu:`Analyze` -> :menu:`Metrics` -> :menu:`Compute Metrics on Current Project`
  * Select **All complexity metrics** and press :menu:`Execute`

.. container:: animate 3-

  * Question 1: What is the average complexity for project?  :ada:`complexity_metrics_example.adb`?

.. container:: animate 4-

  1.3 for the project, 2 for the file

.. container:: animate 5-

  * Question 2: Which file has an statement complexity of 1.5?

.. container:: animate 6-

  :filename:`line_metrics_example.adb`

-----------------------------
GNATmetric Lab - CLI Part 1
-----------------------------

* Use the command line to generate syntax elements metrics for the project

  :command:`gnatmetric -Pdefault.gpr -U --syntax-all`

.. container:: animate 1-

  * Question 1: How many total statements in the project? Declarations?

.. container:: animate 2-

  16 statements, 70 declarations

.. container:: animate 3-

  * Question 2: What are the number of statements and declarations for :ada:`From_String` in package :ada:`Syntax_Metrics_Example`?

.. container:: animate 4-

  3 statements, 4 declarations

  You need to open the file ``obj\syntax_metrics_example.adb.metrix`` to get the data

-----------------------------
GNATmetric Lab - CLI Part 2
-----------------------------

Generate a local version of the combined XML metrics file for coupling metrics without generating any of the text files

.. container:: animate 2-

  ``gnatmetric -Pdefault.gpr -U --coupling-all --no-text-output --xml-file-name=.\local.xml``

.. container:: animate 3-

  * Question: How many total lines in the generated XML file?

.. container:: animate 4-

  100
