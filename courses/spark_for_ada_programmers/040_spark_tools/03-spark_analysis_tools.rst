======================
SPARK Analysis Tools
======================

---------------------------------------------
:toolname:`GNATprove` - A Command Line Tool
---------------------------------------------

* Invocation syntax: :command:`gnatprove -P prj-file [switches]`
* If project file not given, like :toolname:`GPRbuild`:

  - Takes the project file in the **current directory** if present
  - Otherwise generates a basic project file

* See all switches with: :command:`gnatprove --help`

  - Basic switches such as number of processors to use

    + Analysis modes with :command:`--mode=`
    + Reporting mode with :command:`--report=`
    + Warnings mode with :command:`--warnings=`
    + Proof level with :command:`--level=0/1/2/3/4`

  - Advanced switches for **fine-grained** control

    + Prover selection with :command:`--prover=`
    + Prover control with :command:`--timeout= --steps= --memlimit=`

--------------------------------------------
:toolname:`GNATprove` - Project File Usage
--------------------------------------------

* Tool package :code:`Prove` corresponds to :toolname:`GNATprove`

  - Use attribute :code:`Proof_Switches` to apply tool-defined switches

    - For all files with value :ada:`"Ada"`
    - For specific file with its name

  .. code:: Ada

     project Proj is
       package Prove is
         for Proof_Switches ("Ada") use ("--level=2");
         for Proof_Switches ("file.adb") use ("--level=3");
       end Prove;
     end Proj;

  - Use attribute :code:`Proof_Dir` to specify directory for session files

----------------------------------------------
Setting the Default :ada:`SPARK_Mode` Value
----------------------------------------------

* Set :ada:`SPARK_Mode` in a global/local configuration pragmas file
  :filename:`config.adc`

  .. code:: Ada

     pragma SPARK_Mode (On);

* Set the :code:`Global_Configuration_Pragmas` attribute in the project file

  .. code:: Ada

     project Proj is
        package Builder is
           for Global_Configuration_Pragmas use "config.adc";
        end Builder;
     end Proj;

----------------------------------------
Adapting the Project File for Analysis
----------------------------------------

* If needed, define a **project variable** to control sources, compilation
  switches, etc.

  .. code:: Ada

     type Modes is ("Compile", "Analyze");
     Mode : Modes := External ("MODE", "Compile");
     case Mode is
        when "Compile" =>
           for Source_Dirs use (...);
        when "Analyze" =>
           for Source_Dirs use ("dir1", "dir2");
           for Source_Files use ("file1.ads", "file2.ads");
     end case;

* Run :toolname:`GNATprove` with appropriate value of :code:`MODE` defined in
  the environment or on the command-line

  .. code:: Ada

     gnatprove -P my_project -XMODE=Analyze

------------------------------------
Structure of :toolname:`GNATprove`
------------------------------------

|

.. image:: spark_structure.png

.. container:: speakernote

   Image comes from Appendix of SPARK User's Guide on "SPARK Architecture,
   Quality Assurance and Maturity".

-------------------
Legality Checking
-------------------

* **First step** in analysis
* :toolname:`GNATprove` does only that with switch :command:`--mode=check_all`
* Error messages on violations

  - Need to fix to go beyond this step

  - Ex: :command:`<expr> cannot depend on variable input <var>`

  - May include fix:

    .. container:: latex_environment tiny

       :color-red:`use instead a constant initialized to the
       expression with variable input`

    |rightarrow| apply the suggested fix

  - May include *explain code*:

    :color-red:`[E0007]`

    |rightarrow| run :command:`gnatprove --explain=E0007` for more information

* Includes ownership checking, detailed in course on Pointer Programs

---------------
Flow Analysis
---------------

* :dfn:`Flow analysis` is a prerequisite to proof
* :toolname:`GNATprove` does that with switch :command:`--mode=flow`

  - This follows legality checking

* Corresponds to :menu:`Examine` menus in IDEs
* :toolname:`GNATprove` applies flow analysis to each subprogram separately

  - Notion of dependency contracts summarize effects of call

* Outputs messages:

  - Error messages need to be fixed
  - Check messages need to be reviewed, and either fixed or justified
  - Warnings can be inspected and silenced

-------
Proof
-------

* :dfn:`Proof` is the final step
* :toolname:`GNATprove` does it all with switch :command:`--mode=all` (the
  default)

* Corresponds to :menu:`Prove` menus in IDEs
* :toolname:`GNATprove` applies proof to each subprogram separately

  - Notion of functional contracts summarize effects of call

* Outputs messages:

  - Check messages need to be reviewed, and either fixed or justified
  - Warnings can be inspected and silenced

------------------------
Categories of Messages
------------------------

* :dfn:`Error messages` start with :command:`error:`

  - :toolname:`GNATprove` aborts analysis and exits with error status

* :dfn:`Check messages` start with severity :command:`high:`,
  :command:`medium:` or :command:`low:`

  - With switch :command:`--checks-as-errors=on`, :toolname:`GNATprove` exits
    with error status

* :dfn:`Warnings` start with :command:`warning:`

  - With switch :command:`--warnings=error`, :toolname:`GNATprove` exits with
    error status
  - Some warnings are guaranteed to be issued

* :dfn:`Information messages` start with :command:`info:`

  - Report proved checks with switch :command:`--report=all`
  - Report information about analysis with switch :command:`--info`

----------------------------------------
:toolname:`GNATprove` Output for Users
----------------------------------------

.. image:: gnatprove-output-options.png

-------------------------------------------------
Analysis Summary File :filename:`gnatprove.out`
-------------------------------------------------

* Located in :filename:`gnatprove/` under project object dir
* An overview of results for all checks in project
* Especially useful when results must be documented
* Details in SPARK User's Guide

|

.. image:: gnatprove-output-file.jpeg
   :width: 60%

