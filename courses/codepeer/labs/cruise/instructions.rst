:title: :toolname:`CodePeer` - Advanced Configuration
:subtitle: Using :toolname:`GPRBuild` options

.. highlight:: ada

**************
Introduction
**************

* Start :toolname:`GNAT Studio`
* Start a baseline run
* Analyze summarily the result

******************
Removing Sources
******************

The Legacy module is raising the majority of error, and its code is using many dangerous constructs

As a result we will remove the legacy module from analysis.

* Open the project file from the :menuselection:`Project` tab, by right-clicking on the :menuselection:`Cruise -> Project -> Edit Source File`.
* Ignore the `Cruise.Legacy` package in analysis

In :toolname:`GPRBuild` you can set the `CodePeer.Excluded_Source_Files` attribute

.. code:: Ada

    package CodePeer is
        for Excluded_Source_Files use ...

* Run a new analysis

*****************
Change Settings
*****************

* Change the default level of analysis to 2
* Add multiprocessing to "0" cores (ie. let the tool chose)

The switch for multiprocessing is `-j`

The :toolname:`GPRBuild` attribute to set the switches is `CodePeer.Switches`

* Run a new analysis with :menuselection:`CodePeer -> Analyze All`

The analysis should return fewer result.

****************
Delta Analysis
****************

* Perform the analysis so that only changes are displayed

Use the switches :command:`-show-added` and :command:`-show-removed`

* Run a new analysis

You should see no message

* Fix a bug on :file:`cruise.adb`
* Run a new analysis

You should see that the bug is now present, as **removed**
