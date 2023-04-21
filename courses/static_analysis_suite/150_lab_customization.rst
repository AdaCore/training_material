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
* Analyze the result

===================
Filtering Sources
===================

-----------------------
Focusing Our Analysis
-----------------------

The Legacy module is raising the majority of error, and its code is using many dangerous constructs

As a result we want to focus on the module that is not legacy.

* In the locations tab, enter the name of :file:`cruise.adb` in the filter text box

.. image:: codepeer/cruise_filter_in.png

* This shows only messages for the specified file

--------------------
Filtering Messages
--------------------

* Open the filter advanced menu by clicking on the magnifying glass icon in the filter bar.

.. image:: codepeer/cruise_filter_advanced_button.png

.. image:: codepeer/cruise_filter_advanced_menu.png

Notice that you have several options there to filter with advanced messages criterias, as well as a search history.

We want to sort the messages by importance, which is called sub-category in the tool.

* Click on the "hamburger menu on the right hand side of the search bar.

.. image:: codepeer/cruise_filter_configuration.png

* Select "Sort by subcategory"

Notice that the messages are now displayed as high category first, then medium.

------------------------
Filtering Source Files
------------------------

These filtering options on existing analysis are useful, but now we want to simply ignore the legacy files when performing the analysis.

* Open the project file from the :menu:`Project` tab, by right-clicking on the :menu:`Cruise -> Project -> Edit Source File`.

.. image:: codepeer/radar_open_gpr.png

* Ignore the `Cruise.Legacy` package in analysis

In :toolname:`GPRBuild` you can set the `CodePeer.Excluded_Source_Files` attribute

You can follow the **template** below to do it

.. code:: Ada

    package CodePeer is
        for Excluded_Source_Files use ("file_to_exclude.ads", "second_file_to_exclude.adb");
    end CodePeer;

-------------------------------
Analysis with Filtered Source
-------------------------------

* Run a new analysis
 
Check that the legacy files do not appear in the result anymore.

* In the Message History menu of the CodePeer Report, check only "Removed"
* Several warnings should be displayed
* Press the "+" or arrow to the left of the cruise project

The result should show that those removed warnings belonged to :file:`cruise-legacy.adb`

.. image:: codepeer/cruise_exclude_legacy.png

=================
Change Settings
=================

-------------------------------
Settings Via The Project File
-------------------------------

We want to set the default value of the CodePeer parameters in the GPR file.

The :toolname:`GPRBuild` attribute to set the switches is `CodePeer.Switches`

* Change the default level of analysis to 2.

The corresponding switch is `--level`

* Add multiprocessing to "0" cores (ie. let the tool chose).

The corresponding switch is `-j`

* Run a new analysis with :menu:`CodePeer -> Analyze All`

The analysis should return fewer result.

----------------------------------
Overriding Project File Settings
----------------------------------

* Configure a new analysis with :menu:`CodePeer -> Analyse...`

In the opened window, you can modify the command used to call CodePeer directly at the bottom of the screen.

.. image:: codepeer/cruise_analysis_manual_switches.png

Notice that there is also an history for this bar.

* Remove the `--level` switch to use the default level.

===================
Tools Integration
===================

---------------
GNAT Warnings
---------------

CodePeer has integration to several tools, namely the GNAT compiler Warnings and GNATcheck.

We will use a bit of the GNAT warnings integration there.

* Configure a new analysis with :menu:`CodePeer -> Analyse...`
* The *Warnings ...* choice is actually a **button**, click on it

.. image:: codepeer/cruise_analysis_warnings_button.png

A new menu appears with a list of warnings to set.

.. image:: codepeer/cruise_analysis_warnings_menu.png

The full list of available GNAT Warnings can be found at https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/building_executable_programs_with_gnat.html#warning-message-control
Depending on your version of the compiler and target, some warnings will or will not be available.

The warning are sorted in the order of **their switch**.
Eg. "Most optional warnings" `-gnatwa` is first, then "Failing assertions" `-gnatw.a`...

---------------------------------
Configure Which Warnings To See
---------------------------------

Each warning can be either unselected, or a bar or a checkmark

The bar indicates that the warning is implicitly checked by CodePeer.

.. image:: codepeer/cruise_analysis_warnings_bar.png

The checkmark indicates that the warning is explicitly checked by CodePeer.

.. image:: codepeer/cruise_analysis_warnings_check.png

Notice that there is a **scrollbar** on the right.

* Enable the GNAT Warnings for

  - Variables that could be constant (`-gnatwk`)
  - Unused entities (`-gnatwu`)

Notice that the called command line now has `--gnat-warnings=uk`

* Run the analysis

You should see numerous warnings added to the messages.

================
Delta Analysis
================

----------------------------
Checking Only Changed Code
----------------------------

* Perform the analysis so that only changes are displayed

Use the switches :command:`--show-added` and :command:`--show-removed`

* Run a new analysis

You should see no message

* Fix a bug on :file:`cruise.adb`
* Run a new analysis

You should see that the bug is now present, as **removed**
