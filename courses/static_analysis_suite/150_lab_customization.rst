*********************
Lab - Customization
*********************

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
Lab Setup
==============

-------------------
Customization Lab
-------------------

* Use :toolname:`GNAT Studio` to open the :filename:`default.gpr` project in the :filename:`150_lab_customization` folder
* Start a baseline run at **level 3**
* Examine the result

===================
Filtering Sources
===================

-----------------------
Focusing Our Analysis
-----------------------

The Legacy module is raising the majority of the errors, and its code is using many dangerous constructs

As a result we want to focus on the module that is not legacy.

* In the locations tab, enter the name of :file:`cruise.adb` in the filter text box

.. image:: codepeer/cruise_filter_in.png

* This shows only messages for the specified file

------------------
Locations Filter
------------------

* Open the filter advanced menu by clicking on the magnifying glass icon in the filter bar.

  .. image:: codepeer/cruise_filter_advanced_button.png
    :width: 50%

  .. image:: codepeer/cruise_filter_advanced_menu.png
    :width: 50%

  * Notice that you have several options there to filter with advanced messages criterias, as well as a search history.

--------------------
Filtering Messages
--------------------

* We want to sort the messages by importance, which is called sub-category in the tool.

  * Click on the "hamburger" menu on the right hand side of the search bar.

.. image:: codepeer/cruise_filter_configuration.png

  * Select "Sort by subcategory"

    * Notice that the messages are now displayed as high category first, then medium.

------------------------
Filtering Source Files
------------------------

* Filtering options on existing analysis are useful, but now we want to simply ignore the legacy files when performing the analysis.

  * Open the project file from the :menu:`Project` tab

    *  Right-click on :menu:`Default` |rightarrow| :menu:`Project` |rightarrow| :menu:`Edit Source File`.

      .. image:: codepeer/cruise_edit_gpr_file.png

* Ignore the :ada:`Cruise.Legacy` package in analysis

  * In :toolname:`GPRBuild` you can set the :ada:`CodePeer.Excluded_Source_Files` attribute
  * Use this as a template

    .. code:: Ada

        package CodePeer is
            for Excluded_Source_Files use ("file_to_exclude.ads",
                                           "second_file_to_exclude.adb");
        end CodePeer;

-------------------------------
Analysis with Filtered Source
-------------------------------

* Run a new analysis and verify legacy files do not appear in the result anymore.

  * In the Message History menu of the CodePeer Report, check only "Removed"
  * Several warnings should be displayed
  * Expand the :ada:`Cruise` project to see all files

* The result should show that those removed warnings belonged to :file:`cruise-legacy.adb`

.. image:: codepeer/cruise_exclude_legacy.png

=================
Change Settings
=================

-------------------------------
Settings Via The Project File
-------------------------------

.. container:: columns

  .. container:: column

      * We want to set the default value of the CodePeer parameters in the GPR file.

      * Change default level of analysis to **2**

          * Corresponding switch is ``--level``

      * Set multiprocessing to **0** cores (ie. let the tool choose)

          * Corresponding switch ``-j``

      * Run new analysis with :menu:`CodePeer` |rightarrow| :menu:`Analyze All`

      * Analysis should return fewer results

  .. container:: column

    .. container:: latex_environment tiny

      ::

        project Default is

           for Languages use ("ada");
           for Source_Dirs use ("src");
           for Object_Dir use "obj";

           package Compiler is
              for Switches ("ada") use ("-gnatwe",
                                        "-gnato",
                                        "-g");
           end Compiler;

           package Codepeer is
              for Switches use ("--level", "2", "-j0");
           end Codepeer;

        end Default;


----------------------------------
Overriding Project File Settings
----------------------------------

* Configure a new analysis with :menu:`CodePeer` |rightarrow| :menu:`Analyze...`

* In the opened window, you can modify the command used to call CodePeer directly at the bottom of the screen.

.. image:: codepeer/cruise_analysis_manual_switches.png

* Notice there is also history for the filter

  * Remove the :ada:`--level` switch to use the default level.

===================
Tools Integration
===================

---------------
GNAT Warnings
---------------

* CodePeer has integration to several tools, namely the GNAT compiler Warnings and GNATcheck.
* We will use a bit of the GNAT warnings integration there.

* Configure a new analysis with :menu:`CodePeer` |rightarrow| :menu:`Analyze...`

   * The *Warnings ...* choice is actually a **button**, click on it

.. image:: codepeer/cruise_analysis_warnings_button.png

* A new menu appears with a list of warnings to set.

.. image:: codepeer/cruise_analysis_warnings_menu.png

  * Full list of GNAT Warnings can be found in *GNAT User's Guide*
  * Depending on your version of the compiler and target, some warnings will or will not be available.
  * Warning are sorted in the order of their switch

    * Eg. "Most optional warnings" :ada:`-gnatwa` is first, then "Failing assertions" :ada:`-gnatw.a`...

---------------------------------
Configure Which Warnings To See
---------------------------------

* Each warning can be either unselected, or a bar or a checkmark

  .. image:: codepeer/cruise_analysis_warnings_bar.png
    :width: 50%

  * Bar - warning is implicitly checked by CodePeer.

  .. image:: codepeer/cruise_analysis_warnings_check.png
    :width: 50%

  * Checkmark - warning is explicitly checked by CodePeer.

* Enable GNAT Warnings for

  - Variables that could be constants (`-gnatwk`)
  - Unused entities (`-gnatwu`)

* Notice that the called command line now has :ada:`--gnat-warnings=ku`

* Run the analysis

  * You should see numerous warnings added to the messages.

================
Delta Analysis
================

----------------------------
Checking Only Changed Code
----------------------------

* Perform the analysis so that only changes are displayed

  * Use the switches :command:`--show-added` and :command:`--show-removed`

* Run a new analysis

  * You should see no message

* Fix a bug in :file:`cruise.adb`
* Run a new analysis

  * Bug now present but listed as **removed**
