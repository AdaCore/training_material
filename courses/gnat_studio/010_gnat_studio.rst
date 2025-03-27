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
+ Available on Linux, Windows
+ Supports native, cross, and bare-board development

  + Same look-and-feel

+ Provides fully symbolic source-level debugging
+ Supports Ada 2012 and all prior versions
+ Supports C, C++ and Python

-----------------
GNAT Studio IDE
-----------------

.. image:: gnat_studio/welcome_to_gnat_studio.jpg

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

.. image:: gnat_studio/project_perspective.jpg

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

.. image:: gnat_studio/syntax_highlighting.jpg

---------------------------------------
Line / Block / Delimiter Highlighting
---------------------------------------

.. image:: gnat_studio/line_block_delimiter_highlighting.jpg

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

.. image:: gnat_studio/completion_examples_for_packages.jpg
   :width: 50%

-------------------------------
Filtered Completion Proposals
-------------------------------

.. image:: gnat_studio/completion_proposals_filtered.jpg
   :width: 50%

-------------------------------------
Information In Subprogram Proposals
-------------------------------------

.. image:: gnat_studio/information_in_subprogram_proposals.jpg

------------------------------
Formal Parameter Completions
------------------------------

.. image:: gnat_studio/completion_formal_parameters.jpg

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

------------
Outline view
------------

.. image:: gnat_studio/outline_view.png

-------------------------------------
Editor's Contextual Navigation Menu
-------------------------------------

.. image:: gnat_studio/editor_contextual_menu.jpg

------------------
Tool-Tip Example
------------------

.. image:: gnat_studio/tooltip_example.jpg

------------------------------------------
Viewing Predefined and GNAT Source Files
------------------------------------------

.. image:: gnat_studio/help_gnat_runtime.jpg

-----------
Call tree
-----------

.. image:: gnat_studio/call_trees.png

======================
Running Applications
======================

-----------------------
Building Applications
-----------------------

+ Uses multi-language builder :toolname:`GPRbuild` by default

  + Ada, C, C++, assembly, user-defined

+ Supports any compiler callable on command line

  + Built-in support for ``GNAT``, ``gcc``, and ``make``

+ Provides easy navigation through error messages
+ Provides automatic "code-fixing"

  + Manually invoked

.. image:: gnat_studio/fix_missing_semi.jpg
   :width: 50%

---------------------------------
Integration with External Tools
---------------------------------

+ Common GUI for version control systems

  + Predefined support for many version control systems
  + Manual integration allowed for other tools

+ GNAT-specific tools, if installed

  + :toolname:`GNAT SAS`
  + :toolname:`SPARK`
  + :toolname:`GNATtest`
  + :toolname:`GNATcoverage`
  + Etc.

+ User-defined tools, with menu entries if needed

========================
Debugging Applications
========================

--------------------
Symbolic Debugging
--------------------

+ Built in to :toolname:`GNAT Studio` as a different "perspective"

  + Additional views, menu entries, and toolbar icons

+ A graphical interface to :toolname:`GDB`
+ Uses a :toolname:`GDB` enhanced to be Ada-aware

  + Task states, not just thread states
  + Advanced types' representations

+ Same interface for native, cross, bare-board

  + Some targets may require target-specific setup

+ Includes a :toolname:`GDB` console for interactive commands

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

.. image:: gnat_studio/debug_task_view.jpg
   :width: 50%

-------------------------------------------
:toolname:`GNAT Studio` Debug Perspective
-------------------------------------------

.. image:: gnat_studio/debug_perspective.jpg

-----------------------
The Debugging Toolbar
-----------------------

.. image:: gnat_studio/debug_toolbar.jpg

-------------
Data Window
-------------

+ Displays values and their relationships in a table
+ Each value is displayed in its own row
+ Each row contains:

  + Name of the expression or variable

    + Components / components can be expanded

  + Value
  + Type (Ada type definition)

-----------------------------------------------------
:toolname:`GNAT Studio` Active In Debug Perspective
-----------------------------------------------------

.. image:: gnat_studio/debug_active_perspective.jpg

==================
Workflow Example
==================

----------------------------------
Starting :toolname:`GNAT Studio`
----------------------------------

+ From the desktop:

  + Double-click on the "project gpr" file icon in a file browser

      .. image:: gnat_studio/start_from_file_explorer.png
         :width: 25%

  + Or start :toolname:`GNAT Studio` and use the Welcome Screen to select project

+ From the command line:

  + Change to the directory containing the project file
  + Enter :command:`gnatstudio` on the command line

----------------------------------------
:toolname:`GNAT Studio` Welcome Screen
----------------------------------------

.. image:: gnat_studio/welcome_dialog.jpg

+ Choose :menu:`Open Project`

  + Click :menu:`Browse` and go to your "dev" directory if the correct directory is not already indicated, or enter it directly

+ Click :menu:`OK`

----------------------
Building Executables
----------------------

.. columns::

   .. column::

      + Press F4 (for first main in list)
      + Or use "Build Main" icon

         .. image:: gnat_studio/build_main_icon.jpg

   .. column::

      + Or click :menu:`Build` :math:`\rightarrow` :menu:`Project` :math:`\rightarrow` *main unit name*

         .. image:: gnat_studio/build_project_main_menu.jpg

---------------------------------
Chance To Change Build Switches
---------------------------------

+ May be displayed when build is invoked
+ Just press OK

   .. image:: gnat_studio/build_switches_dialog.jpg
         :width: 50%

-----------------------------------------
Error In Source File and Locations View
-----------------------------------------

.. image:: gnat_studio/build_error.jpg

---------------------
Results of Building
---------------------

+ Any error lines are displayed against a colored background in the source window
+ *Locations* window displays error messages
+ *Messages* window gives tool output results

----------------------------
Using the Locations Window
----------------------------

+ Can click on a line to go to that source location
+ Click on the "wrench" icon to apply Code Fix

.. image:: gnat_studio/location_windows_wrench.jpg

------------------------------------
Result of Code Fix via Wrench Icon
------------------------------------

.. image:: gnat_studio/after_using_wrench.jpg

--------------------------------
Build the Executable After Fix
--------------------------------

+ Press F4 (for first main in list)
+ Or use "Build Main" icon
+ Or click :menu:`Build` :math:`\rightarrow` :menu:`Project` :math:`\rightarrow` *main unit name*

---------------------
Running The Program
---------------------

+ Click :menu:`Build` :math:`\rightarrow` :menu:`Run` :math:`\rightarrow` *main unit name*
+ Leave *Use external terminal* unchecked
+ Press :menu:`Execute`

.. image:: gnat_studio/run_the_program.jpg

-----------------------
(Internal) Run Window
-----------------------

.. image:: gnat_studio/internal_run_window.jpg

------------------------------------------
When Multiple Mains Are Defined
------------------------------------------

.. columns::

   .. column::

      + Build Icon

         .. image:: gnat_studio/build_multiple_icon.jpg

   .. column::

      + Run Icon

         .. image:: gnat_studio/run_multiple_icon.jpg

-----------------------------------
Help With :toolname:`GNAT Studio`
-----------------------------------

.. image:: gnat_studio/help_menu_cascade.jpg

------------------------------------
About :toolname:`GNAT Studio` Help
------------------------------------

+ Information on :toolname:`GNAT Studio`

  + Welcome (gets you to the Tutorial and the Users Guide)
  + Contents (which includes links to your reference manuals for GNAT, GDB and GCC, etc.)

+ Information on other tools and capabilities

  + GNAT
  + :toolname:`GNAT SAS`
  + GNU tools
  + GNAT Runtime
  + Python Extensions

+ All GNATPro tools have a command-line argument :command:`--help`

--------------------------
User Guides and Examples
--------------------------

.. image:: gnat_studio/users_guides_and_examples.jpg

===============================
Using Version Control Systems
===============================

-------------------------
What is version control
-------------------------

+ System that records changes

  + to a file or set of files
  + over time
  + recall specific versions later 
  + revert selected files back to a previous state
  + compare changes over time
  + who introduced an issue, and when

+ GNAT Studio Supports many Version Control Systems (VCS)

  + Git 
  + Subversion
  + CVS
  + Rational Clearcase
  + Mercurial

-------------
What is Git
-------------

+ A VCS

  + Used to demo the GNAT Studio VCS Features

+ Distributed

  + No single database of reference
  + Most operations don't require a server

+ Integrity checks
+ 3 states

.. image:: gnat_studio/git_3_states.png

-----------------------------------
GNAT Studio interface for Staging
-----------------------------------

.. image:: gnat_studio/vcs_staging.png

+ Tip: Renaming = Removing a file and creating a new file with the same content

.. image:: gnat_studio/vcs_staging_showing_changes.png

-----------
File Diff
-----------

+ Clicking on a file opens a diff

.. image:: gnat_studio/vcs_diff.png

-----------------------------
Actions on the staging area
-----------------------------

.. image:: gnat_studio/vcs_actions.png

+ Local

  + Undo local change(s)
  + Commit
  + Merge

+ Distant

  + Push
  + Fetch
  + Pull = Fetch + Merge

---------------------
Commit a local change
---------------------

.. image:: gnat_studio/commits_view_add_commit.svg

========
Lab
========

.. include:: labs/010_gnat_studio.lab.rst
