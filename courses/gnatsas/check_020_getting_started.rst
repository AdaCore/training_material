*****************
Getting Started
*****************

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

=============
Basic Usage
=============

-------------------------
Command Line Invocation
-------------------------

.. container:: latex_environment scriptsize

  :command:`gnatcheck [options] {filename} {-files=filename} [-cargs gcc_switches] -rules rule_switches`

.. list-table::
   :header-rows: 1

   * - Argument

     - Description

   * - {filename}

     - File to analyze (wildcards allowed)

   * - {files=filename}

     - :filename:`filename` specifies text file containing list of files to analyze

   * - -rules rule_switches

     - Rules to apply for analysis

Where ``rule_switches`` can be any combination of the following:

  .. list-table::
     :header-rows: 1

     * - Switch

       - Explanation

     * - -from=filename

       - read rule options from :filename:`filename`

     * - +R<rule_id>[:param]

       - turn ON a given rule [with given parameter]

     * - -R<rule_id>

       - turn OFF a given rule

     * - -R<rule_id>:param

       - turn OFF some of the checks for a given  rule,

     * - 

       - depending on the specified parameter

--------------------------
Command Line Example Run
--------------------------

:command:`gnatcheck -P simple.gpr -rules -from=coding_standard.rules`

::

   chop.adb:14:11: PIck_Up does not have casing specified (mixed)
   chop.ads:11:18: Stick does not start with subtype prefix T_
   phil.adb:21:11: Think_Times does not start with subtype prefix T_
   phil.adb:33:05: "Who_Am_I" is not modified, could be declared constant
   phil.ads:12:03: violation of restriction "No_Tasking"
   phil.ads:12:13: Philosopher does not start with subtype prefix T_
   phil.ads:12:26: My_ID does not have casing specified (mixed)
   phil.ads:19:08: States does not end with type suffix _Type
   phil.ads:19:08: States does not start with subtype prefix T_
   random_generic.ads:5:08: Result_Subtype does not end with type suffix _Type
   random_generic.ads:5:08: Result_Subtype does not start with subtype prefix T_
   room.adb:19:03: violation of restriction "No_Tasking"
   room.adb:19:23: anonymous subtype
   ...

.. container:: latex_environment footnotesize

   *These messages are coming from rules specified in* :filename:`coding_standard.rules`

--------------------------------------------------------------
:toolname:`GNATcheck` From :toolname:`GNAT Studio` Main Menu
--------------------------------------------------------------

:menu:`Analyze` |rightarrow| :menu:`Coding Standard` |rightarrow| :menu:`Check Root Project`

.. image:: gnatcheck/gnatstudio_menu.png

----------------------------------------------------------------
:toolname:`GNATcheck` From :toolname:`GNAT Studio` Right-Click
----------------------------------------------------------------

.. container:: latex_environment scriptsize

  .. container:: columns

    .. container:: column

      * Right-click on project in Project pane

        .. image:: gnatcheck/gnatstudio_rightclick_project.png
          :width: 40%

      * Right-click on folder in Project pane

        .. image:: gnatcheck/gnatstudio_rightclick_directory.png
          :width: 40%

    .. container:: column

      Right-click on file in Project pane

        .. image:: gnatcheck/gnatstudio_rightclick_file.png
          :width: 40%

      Right-click in source file editor window

        .. image:: gnatcheck/gnatstudio_rightclick_editor.png
          :width: 40%

-----------------
GUI Example Run
-----------------

.. image:: gnatcheck/results_in_gnatstudio.png

-----------------------
Specifying Rules File
-----------------------

* Rules file can be specified on command line

    * :command:`gnatcheck -rules -from=coding_standard.rules ...`
    * But more commonly defined in project file

.. container:: latex_environment tiny

  .. container:: columns

    .. container:: column

      .. code:: GPRbuild

        project Simple is
           for Source_Dirs use ("./include", "./src");
           for Main use ("diners");
           for Object_Dir use "./obj";
           package Check is
              for Default_Switches ("ada") use
                 ("-rules", "-from=coding_standard.rules");
           end Check;
        end Simple;

    .. container:: column

      :menu:`Edit` |rightarrow| :menu:`Project Properties` |rightarrow| :menu:`Switches` |rightarrow| :menu:`GNATcheck`

      .. image:: gnatcheck/properties_dialog.png

=====
Lab
=====

.. include:: labs/check_020_getting_started.lab.rst

=========
Summary
=========

-----------------
Closing Remarks
-----------------

+ :toolname:`GNATcheck` is a coding standards checker

  + :dfn:`Rules` are how the tool decides what is the "standard"

+ Rules are broken down into two categories:

  + Predefined rules - over 200 rules built into the tool
  + User-defined rules - ability for user to write their own rules
