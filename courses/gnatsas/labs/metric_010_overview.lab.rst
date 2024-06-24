----------------------
GNATmetric Lab Setup
----------------------

* Copy the :filename:`tutorial` folder from the course materials location

* Contents of the tutorial folder:

  * :filename:`sdc.gpr` - project file
  * :filename:`common` - source directory
  * :filename:`struct` - source directory
  * :filename:`obj` - object file (and metrics results) directory 

* From a command prompt, type :command:`gnatmetric --help` to verify your path is set correctly

  * If not, add the appropriate :filename:`bin` directory to your path
  * Typically (for Windows), this is located in ``C:\GNATSAS\<version>\bin``

-----------------------------
GNATmetric Lab - GUI Part 1
-----------------------------

* Use :toolname:`GNAT Studio` to open the project :filename:`sdc.gpr`

* Select :filename:`instructions.adb` in the :filename:`struct` folder

* Perform metrics analysis to get all line metrics on this file

  * :menu:`Analyze` -> :menu:`Metrics` -> :menu:`Compute Metrics on Current File`
  * Select **All line metrics** and press :menu:`Execute`

.. container:: animate 1-

  Question 1

.. container:: columns

  .. container:: column

    .. container:: animate 1-

      * How many lines in the file? In subprogram :ada:`Process`?

  .. container:: column

    .. container:: animate 2-

        59 lines in the file

        20 lines in Process

.. container:: animate 3-

  Question 2

.. container:: columns

  .. container:: column

    .. container:: animate 3-

      * Is there any information for the package spec?

  .. container:: column

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

  Question 1

.. container:: columns

  .. container:: column

    .. container:: animate 3-

      * What is the average complexity for the project? :ada:`stack.adb`?

  .. container:: column

    .. container:: animate 4-

        2.3

        1.7

.. container:: animate 5-

  Question 2

.. container:: columns

  .. container:: column

    .. container:: animate 5-

      * Which file has an essential complexity of 1?

  .. container:: column

    .. container:: animate 6-

      :filename:`sdc.adb`

-----------------------------
GNATmetric Lab - CLI Part 1
-----------------------------

* Use the command line to generate syntax elements metrics for the project

  :command:`gnatmetric -Psdc.gpr -U --syntax-all`

.. container:: animate 1-

  Question 1

.. container:: columns

  .. container:: column

    .. container:: animate 1-

      * How many total statements and declarations in the project?

  .. container:: column

    .. container:: animate 2-

        Statements - 160

        Declarations - 195

.. container:: animate 3-

  Question 2

.. container:: columns

  .. container:: column

    .. container:: animate 3-

      * What are the number of statements and declarations for procedure :ada:`Push` in package :ada:`Stack`?

  .. container:: column

    .. container:: animate 4-

      Statements - 5

      Declarations - 2

      You need to open the file ``obj\stack.adb.metrix`` to get the data

-----------------------------
GNATmetric Lab - CLI Part 2
-----------------------------

Generate a local version of the combined XML metrics file for coupling metrics without generating any of the text files

.. container:: animate 2-

  ``gnatmetric -Psdc.gpr -U --coupling-all --no-text-output --xml-file-name=.\local.xml``

.. container:: animate 3-

  Question

.. container:: columns

  .. container:: column

    .. container:: animate 3-

      * How many total lines in the generated XML file?

  .. container:: column

    .. container:: animate 4-

        118
