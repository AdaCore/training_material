-----------------
GNAT Studio Lab
-----------------
   
* Goals
   
   * Using :toolname:`GNAT Studio`, you should be able to:

      * Build a project using existing source files
      * Fix coding issues by hand or automatically
      * Debug executables

* Copy the two source directories (:filename:`common` and :filename:`struct`) to a workarea
 
------------------------------
Create Project - New Project
------------------------------

* Start :toolname:`GNAT Studio` from the command line or the application menu

* In the Welcome dialog, select :menu:`Create new project`

   * Select :menu:`Simple Ada Project` and click :menu:`Next`

      * Fill in **Location** and **Settings** as appropriate
      * Click :menu:`Apply` to build the project

-----------------------------------
Create Project - Project Settings
-----------------------------------

* Select :menu:`Edit` :math:`\rightarrow` :menu:`Project Properties...`

   * Navigate to the **Sources** :math:`\rightarrow` **Directories** tab

      * Remove the pre-populated directory
      * Add the :filename:`common` and :filename:`struct` directories 

   * Navigate to the **Sources** :math:`\rightarrow` **Main** tab

      * Replace the :filename:`main.adb` file with :filename:`sdc.adb`
      * (Clicking the **+** icon brings up a list of all possible files)

   * Navigate to the **Build** :math:`\rightarrow` **Switches** :math:`\rightarrow` **Ada** tab

      * Select *Debug information* (so we can debug later)
      * Under *Warnings*, enable most warnings

   * Click :menu:`Save` to save settings

--------------------------------
Create Project - Build Project
--------------------------------

* Press :menu:`F4` (and then :menu:`Execute`) to build the executable

   * There are errors in the supplied code!

--------------
Error Fixing
--------------

* The error(s) appear in the **Locations** window

   * Clicking on the error line will jump to that line of code
   * For errors which :toolname:`GNAT Studio` can fix, a wrench icon appears

      * In the **Locations** window
      * In the source file window

   * Clicking either of these wrenches should fix the problem

* Continue fixing errors (and warnings) until the executable builds

------------------------
Running the Executable
------------------------

* This example is a simplistic postfix desktop calculator that accepts input from a file or interactively

   * For example, entering :command:`1 2 + print` should give you the result 4, while :command:`12 6 / print` will give you the result 2

* Run the executable via :menu:`Build` :math:`\rightarrow` :menu:`Run` or by pressing the right-pointing triangle icon

   * Enter :command:`1 2 + print` as the command
   * **Internal Error** is not your fault - their is a bug in the code!

--------------------------
Debugging the Executable
--------------------------

* **Internal Error** is printed when an exception is raised - let's try to find it
* Click the bug-like icon (:menu:`Build & Debug`) on the toolbar to start the debugger
* Click the :menu:`Continue` icon to start execution

   * Dialog has checkboxes - make sure *Stop at beginning of main subprogram* is checked so we can set a breakpoint

* Executable stops at main subprogram (*Temporary breakpoint*) 

-----------------------------------------
Debug - Setting an Exception Breakpoint
-----------------------------------------

.. columns::

   .. column::

      * We want to set a breakpoint when an exception is raised
      * In the *Breakpoints* window, click the **+** icon
      * Set the breakpoint type to *break on exception*
      * Press :menu:`OK`
      * Breakpoint appears in the ``Breakpoints`` window
      * Click :menu:`Continue` to enter your data and see the exception

   .. column::

      .. image:: ../../images/gnat_studio/lab_breakpoint_editor.jpg

---------------------------------
Debug - Following an Breakpoint
---------------------------------

* Execution stops where exception is raised

   * Not always in your actual code
   * In **Debugger Console** exception information is presented
   * In **Call Stack** window, you can see where you are in the call stack

      * Click on the first entry that looks like your code

   * To see current value of an object, hover over it

      * To track the value, right-click and select :menu:`Debug` :math:`\rightarrow` :menu:`Display <> in Variables view`
