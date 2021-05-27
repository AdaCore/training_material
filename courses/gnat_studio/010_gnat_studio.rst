*************
GNAT Studio
*************

==============
Introduction
==============

-------------
GNAT Studio
-------------

+ Our flagship IDE
+ Available on Linux, Windows, macOS
+ Supports native, cross, and bare-board development

  + Same look-and-feel

+ Provides fully symbolic source-level debugging
+ Supports Ada 2012 and all prior versions
+ Supports C, C++ and Python

-----------------
GNAT Studio IDE
-----------------

.. image:: ../../images/gnatstudio/welcome_to_gnat_studio.jpg

------------------------------------
Integrated with GNAT Project Files
------------------------------------

+ Graphically presents what the project file specifies

  + Source directories
  + Relationships to other projects
  + Object and executable directories
  + Etc.

+ Builds apps per the project file settings

  + Specified toolchain
  + Switches to be applied

+ GUI for working with *scenario variables*

------------------------------------
GNAT Studio - Project Perspective
------------------------------------

.. image:: ../../images/gnatstudio/project_perspective.jpg

==========
Features
==========

-----------------------------
Configurable and Extensible
-----------------------------

+ Use your own color themes, favorite fonts, etc.
+ Control layout for window panes within the application
+ Create your own actions, with menu entries

  + Written in Python when appropriate

+ Define your own editor text expansions (*aliases*)

  + Parameterized if necessary

+ Define your own keyboard key assignments

  + E.g., binding a key sequence to an existing or user-defined action

-------------------------------------
Provides Language-Sensitive Editing
-------------------------------------

+ Syntax-directed coloring and highlighting

  + Statements, types, annotations, comments, etc.

+ Indentation based on language syntax & surrounding code
+ Automatic formatting as you type

  + Indentation, letter casing, coloring, etc.

+ *Scope folding* to elide syntax-defined blocks of code
+ Refactoring for entity renaming & subprogram extraction
+ Semantics-based completion for both words and constructs

---------------------
Syntax Highlighting
---------------------

.. image:: ../../images/gnatstudio/syntax_highlighting.jpg

---------------------------------------
Line / Block / Delimiter Highlighting
---------------------------------------

.. image:: ../../images/gnatstudio/line_block_delimiter_highlighting.jpg

-----------------------
Automatic Indentation
-----------------------

+ Invoked when pressing enter key
+ Modes

  None
    No indentation performed

  Simple
    Next line indented same as current line

  Extended
    Based on language syntax & surrounding code

+ Modes are controlled by preferences

   + :menu:`Edit` :math:`\rightarrow` :menu:`Preferences` :math:`\rightarrow` :menu:`Editor` :math:`\rightarrow` :menu:`Ada`

+ Also invoked by pressing the indentation key

  + Ctrl-Tab by default
  + Can change via key manager

---------------------------
Textual (Word) Completion
---------------------------

+ Handy since source files often contain many references to the same words
+ Invoked by Ctrl-/ after a partial word

  + Next possible completion will be inserted in the editor
  + Repeating cycles through list of candidate completions

+ Candidates are those words occurring in the edited source file itself
+ Key combination is customizable through the key manager dialog

------------------------------
Smart (Semantic) Completions
------------------------------

+ Completes the identifier prefix under the cursor
+ Lists the results in a pop-up list
+ Offers completions from the entire project
+ Requires enabling *smart completion* preference

  + Hence computation of an entity database at :toolname:`GNAT Studio` startup

+ Allows configuring the time interval before pop-up
+ Invocations

  + Automatically, on a partial word
  + Manually, by hitting control-space
  + Automatically, immediately after a dot
  + Automatically, immediately after an opening (left) parenthesis

----------------------------------------
Smart Completions Example for Packages
----------------------------------------

.. image:: ../../images/gnatstudio/completion_examples_for_packages.jpg

-------------------------------
Filtered Completion Proposals
-------------------------------

.. image:: ../../images/gnatstudio/completion_proposals_filtered.jpg

-------------------------------------
Information In Subprogram Proposals
-------------------------------------

.. image:: ../../images/gnatstudio/information_in_subprogram_proposals.jpg

------------------------------
Formal Parameter Completions
------------------------------

.. image:: ../../images/gnatstudio/completion_formal_parameters.jpg

----------------------------
Supports Source Navigation
----------------------------

+ For Ada, C, and C++
+ Hyperlinks allow project-wide traversal

  + Visiting declaration for a given name, the body of a routine, etc.
  + Including language-defined entities

+ Contextual menus for navigating to current entity
+ Dynamic dispatching calls are highlighted
+ Traversable call graphs show entity relationships

  + E.g., "who calls this routine" or "who depends upon this package"

+ "Tool-tips" pop up to show semantic information

-------------------------------------
Editor's Contextual Navigation Menu
-------------------------------------

.. image:: ../../images/gnatstudio/editor_contextual_menu.jpg

------------------
Tool-Tip Example
------------------

.. image:: ../../images/gnatstudio/tooltip_example.jpg

------------------------------------------
Viewing Predefined and GNAT Source Files
------------------------------------------

.. image:: ../../images/gnatstudio/help_gnat_runtime.jpg

====================================
Running and Debugging Applications
====================================

-----------------------
Building Applications
-----------------------

+ Uses multi-language builder GPRbuild by default

  + Ada, C, C++, assembly, user-defined

+ Supports any compiler callable on command line

  + Built-in support for GNAT, gcc, and make

+ Provides easy navigation through error messages
+ Provides automatic "code-fixing"

  + Manually invoked

.. image:: ../../images/gnatstudio/fix_missing_semi.jpg

---------------------------------
Integration with External Tools
---------------------------------

+ Common GUI for version control systems

  + Predefined support for Git , Subversion, CVS, Rational Clearcase
  + You can create your own tool-specific integration to the GUI

+ GNAT-specific tools, if installed

  + CodePeer
  + SPARK
  + GNATtest
  + GNATcoverage
  + Etc.

+ User-defined tools, with menu entries if needed

--------------------
Symbolic Debugging
--------------------

+ Built in to :toolname:`GNAT Studio` as a different "perspective"

  + Additional views, menu entries, and toolbar icons

+ A graphical interface to GDB
+ Uses a GDB enhanced to be Ada-aware

  + Task states, not just thread states
  + Advanced types' representations

+ Same interface for native, cross, bare-board

  + Maybe an extra setup step for cross or bare-board targets

+ Includes a GDB console for interactive commands

--------------------
Language Sensitive
--------------------

+ Multiple languages supported

  + Ada, C, C++ code in the same application

+ Set variables, display expressions

  + Using language-specific syntax

+ Browse source

  + Including language-defined entities

------------
Extensible
------------

+ You can call functions & procedures interactively

  + Using language-specific syntax
  + Very useful to print program specific info
  + No need to hardcode display routine calls within source

+ Has powerful scripting facility

  + Can execute when app stops at a breakpoint
  + User defined commands (on the fly)
  + Command files (macros useful for your project)

-----------------------------------
Fine-grained & Expressive Control
-----------------------------------

+ Stepping

  + Over source line
  + Into and around subprograms
  + Over a single assembly instruction

+ Breakpoints

  + Conditional & unconditional
  + Can execute a series of commands at breakpoint

+ Viewable call stack

  + Move to any called routine on the call chain

-----------------
Exception Aware
-----------------

+ Halt when a *specific* exception is raised
+ Halt when an *unhandled* exception is raised
+ Halt when *any* exception is raised

----------------------
Tasking/Thread Aware
----------------------

+ View all tasks/threads in the application
+ Set task specific breakpoints
+ Switch among tasks by clicking on view entries

.. image:: ../../images/gnatstudio/debug_task_view.jpg

---------------------------------------------
:toolname:`GNAT Studio` 'Debug' Perspective
---------------------------------------------

.. image:: ../../images/gnatstudio/debug_perspective.jpg

-----------------------
The Debugging Toolbar
-----------------------

.. image:: ../../images/gnatstudio/debug_toolbar.jpg

-----------------
The Data Window
-----------------

+ Graphically displays values and their relationships
+ Each value is displayed in a "box"
+ Each value box contains:

  + Internal identifier of the box
  + Name of the expression or variable
  + Icon representing the update state
  + Main area displaying value in language-sensitive manner

+ Relationships (if any) are depicted between boxes

-----------------------------------------------------
:toolname:`GNAT Studio` Active In Debug Perspective
-----------------------------------------------------

.. image:: ../../images/gnatstudio/debug_active_perspective.jpg

==================
Workflow Example
==================

----------------------------------
Starting :toolname:`GNAT Studio`
----------------------------------

+ From the desktop:

  + Double-click on the "project gpr" file icon in a file browser

      .. image:: ../../images/gnatstudio/start_from_file_explorer.png

  + Or start :toolname:`GNAT Studio` and use the Welcome Screen to select project

+ From the command line:

  + Change to the directory containing the project file
  + Enter :command:`gnatstudio` on the command line

----------------------------------------
:toolname:`GNAT Studio` Welcome Screen
----------------------------------------

.. image:: ../../images/gnatstudio/welcome_dialog.jpg

+ Choose :menu:`Open Project`

  + Click :menu:`Browse` and go to your "dev" directory if the correct directory is not already indicated, or enter it directly

+ Click :menu:`OK`

----------------------
Building Executables
----------------------

+ Press F4 (for first main in list)
+ Or use "Build Main" icon

   .. image:: ../../images/gnatstudio/build_main_icon.jpg

+ Or click Build " Project " *main unit* *name*

   .. image:: ../../images/gnatstudio/build_project_main_menu.jpg

---------------------------------
Chance To Change Build Switches
---------------------------------

+ May be displayed when build is invoked
+ Just press OK

   .. image:: ../../images/gnatstudio/build_switches_dialog.jpg

-----------------------------------------
Error In Source File and Locations View
-----------------------------------------

.. image:: ../../images/gnatstudio/build_error.jpg

---------------------
Results of Building
---------------------

+ Any error lines are displayed against a colored background in the source window
+ "Locations" window displays error messages
+ "Messages" window gives tool output results

----------------------------
Using the Locations Window
----------------------------

+ Can click on a line to go to that source location
+ Click on the "wrench" icon to apply Code Fix

.. image:: ../../images/gnatstudio/location_windows_wrench.jpg

------------------------------------
Result of Code Fix via Wrench Icon
------------------------------------

.. image:: ../../images/gnatstudio/after_using_wrench.jpg

----------------------
Build the Executable
----------------------

+ Press F4 (for first main in list)
+ Or use "Build Main" icon
+ Or click Build " Project " *main unit name*

---------------------
Running The Program
---------------------

+ Click Build " Run " *main unit name*
+ Leave "Use external terminal" unchecked
+ Press Execute

.. image:: ../../images/gnatstudio/run_the_program.jpg

-----------------------
(Internal) Run Window
-----------------------

.. image:: ../../images/gnatstudio/internal_run_window.jpg

------------------------------------------
When Multiple Mains Are Defined
------------------------------------------

+ Build Icon

   .. image:: ../../images/gnatstudio/build_multiple_icon.jpg

+ Run Icon

   .. image:: ../../images/gnatstudio/run_multiple_icon.jpg

-----------------------------------
Help With :toolname:`GNAT Studio`
-----------------------------------

.. image:: ../../images/gnatstudio/help_menu_cascade.jpg

------------------------------------
About :toolname:`GNAT Studio` Help
------------------------------------

+ Information on :toolname:`GNAT Studio`

  + Welcome (gets you to the Tutorial and the Users Guide)
  + Contents (which includes links to your reference manuals for GNAT, GDB and GCC, etc.)

+ Information on other tools

  + GNAT
  + CodePeer
  + GNU tools
  + GNAT Runtime
  + Python Extensions

+ All gnat pro tools have a command-line argument " --help "

--------------------------
User Guides and Examples
--------------------------

.. image:: ../../images/gnatstudio/users_guides_and_examples.jpg

==============================
:toolname:`GNAT Studio` Labs
==============================

-----------------
Exercises Setup
-----------------

+ Go to the source directory tree previously copied
+ Verify 'write' permissions on entire tree
+ **gnatstudio**
+ **common**
+ **objs**
+ **diners**
+ **objs**
+ **dining_philosophers.gpr**
+ **my_proj**
+ **objs**
+ **custom**

-----------------------------
Source Directories For Labs
-----------------------------

+ **sources**
+ **gnat**
+ **projects**
+ *gnatstudio*
+ **diners**
+ **common**
+ **my_proj**
+ **solutions**
+ **common**
+ **amazing**
+ **library**
+ **solutions**
+ **filenames**
+ **custom**
+ **Use if necessary**

----------------------------
CodeFix, Build and Run Lab
----------------------------

+ Open the Dining Philosophers project if not open
+ Press F4 key to build the executable
+ The Locations Window appears, showing a syntax error and the source editor opens the file

  + If not visible, drag the controls above the Messages window upward

+ Left-click on the wrench in the error message line to apply the fix
+ Press F4 again (no need to save the file manually)
+ Run the program (internally) for 5 seconds

  + Enter 5 at the prompt

+ Close :toolname:`GNAT Studio`

------------------------
Tutorial Debugging Lab
------------------------

+ Copy the :toolname:`GNAT Studio` "tutorial" directory tree to your local workspace (where you have full permissions)

  + Under the :toolname:`GNAT Studio` *installation-root*
  + On Windows, for example:
  + On Linux, for example:

+ Start :toolname:`GNAT Studio` on the tutorial/ sdc.gpr file
+ Within :toolname:`GNAT Studio`, open the :toolname:`GNAT Studio` Tutorial document via the Help menu entry
+ Perform the "Building applications" section
+ Then follow the "Debug" section steps
+ C:\GNATPRO\21.1\share\examples\gnatstudio\tutorial\
+ /opt/gnatpro/21.1/share/examples/gnatstudio/tutorial/

-------------------------------
Creating New Projects Lab (1)
-------------------------------

+ Use the project wizard to create a new "Single Project" by selecting "Project->New..." to invoke the wizard
+ Name the new project "test" (without the quotes)
+ Specify that the new project file will be located in gnatstudio/my_proj under the sources directory tree
+ Select Ada for the only language
+ The sources to be included in the project are located in the gnatstudio/my_proj and gnatstudio/common directories

-------------------------------
Creating New Projects Lab (2)
-------------------------------

+ Use no version control
+ Use gnatstudio/my_proj/objs for the Build directory
+ Use "." (no quotes) for the Exec directory
+ For the main program, use  "fibonacci.adb" located in the gnatstudio/my_proj directory
+ Use the default naming scheme

-------------------------------
Creating New Projects Lab (3)
-------------------------------

+ The rest of the wizard default settings can be used simply by pressing the "Forward" button in successive wizard dialog pages
+ In the last wizard dialog page press "Apply" to create the project with your settings
+ If everything worked, press F4 to build the executable as a test
