**********
GNATtest
**********

.. include:: support_files/symbols.rst

----------
GNATtest
----------

* An **automatic** unit test **harness** generator

    + Including driver infrastructure
    + Including stubs

* Handles large, complex projects
* Can integrate **external** tests
* Supports contract-based programming tests
* Helps verify OOP requirements of DO-178C
* Integrated into :toolname:`GNAT Studio`
* Supports native, cross & high-integrity platforms

---------------------------
Why Automate the Process?
---------------------------

* Developing tests is **labor-intensive**
* Much of the effort is **not specific** to the tests

    - Developing the harness and driver

        + eg. How to test class hierarchy?

    - **Verifying** output is as expected
    - **Maintenance** and update when new units to be tested

* Using :toolname:`GNATtest` developers are **focused**

    + Test **cases**
    + Test **data**
    + **High-value** part

------------------------
What Can Be Automated?
------------------------

.. image:: images/gnattest/what_can_be_automated.jpg

-------------------
A Libadalang Tool
-------------------

* Syntax **needs** to be correct
* Sources **should** be compilable

    - Else generated code won't be

* Sources **should** be complete
* Preprocessing is **limited**

    - Complex preprocessing will cause issues

----------------------------
Based On :toolname:`AUnit`
----------------------------

* Unit test **framework**

    - Based on CppUnit for C++

* Generate the unit-tests' **boilerplate** code

    - Test **harness**
    - Suites
    - Default (*skeleton*) test cases
    - Compilable **as-is**

* More information on :toolname:`AUnit`

    - Tutorials serie by Daniel Bigelow
    - http://www.youtube.com/user/DanielRBigelow

* Installation required for :toolname:`GNATtest`

-------------------------
Command Line Invocation
-------------------------

.. container:: latex_environment footnotesize

:command:`gnattest -P <project-filename> {switches} [filename] [-cargs GCC_switches]`

* :command:`<project-filename>`

    - Name of the GNAT **project file**

* :command:`{switches}`

    - :toolname:`GNATtest`-specific switches

* :command:`[filename]`

    - File containing package to test
    - Default: generate for all sources

* :command:`[-cargs GCC_switches]`

    - Switches passed to **compiler** invocations

* Examples:

    :command:`gnattest -P foo.gpr src/repository.ads`
    :command:`gnattest -P foo.gpr`

-------------------
Generated Outputs
-------------------

* Automatic **harness** code

    - Driver **infrastructure**
    - Can be destroyed and **regenerated** at will
    - Manual changes can be **overwritten** by the tool
    - You **should not** modify this code manually

* Unit test **skeletons**

    - **Actual** unit-test code
    - One for each visible **subprogram** in tested packages
    - **Manually** modified for specific tests' logic
    - **Created** automatically if they don't exist
    - Otherwise **never** overwritten

-------------------------------------
Default Directory Names & Locations
-------------------------------------

* Under the project's **object** directory

    - **Harness** code in :filename:`gnattest/harness/`
    - Unit test **skeletons** in :filename:`gnattest/tests/`

.. columns::

   .. column::

      .. include:: examples/simple_stubbing/extracts/ops.object_dir.gpr
         :code: Ada

   .. column::

      .. image:: images/gnattest/object_directory_hierarchy.jpg

--------------------------
Switches for the Harness
--------------------------

:command:`--harness-dir=<dirname>`

    - Directory holding the **harness** packages and the test **driver**'s project file
    - :filename:`<dirname>` **may** be relative to the object directory

:command:`--passed-tests=<value>`

    - Show **passed tests** when they succeed
    - :code:`<value>` is either :code:`show` (the default) :code:`hide` (lowercase)

----------------------------
**Miscellaneous Switches**
----------------------------

:command:`-r`

    - Process **recursively** all sources from all the project tree
    - Directories are generated for **each project**, local to each

:command:`-X<name>=<value>`

    - Set external variable :code:`<name>` to :code:`<value>` for current invocation

:command:`-q`

    - Suppresses **noncritical** output messages

:command:`-v`

    - Verbose mode: generates **additional** output
    - When run alone display **version** information

----------------------------------------
Are The Default Locations Good Enough?
----------------------------------------

* The defaults are **workable**

    - With **caveats**

* **Object** directory contents are conceptually **transient**
* Test **harness**

    - **Never** manually modified
    - Regenerated **automatically**
    - **Transient**

    * Can stay under the **object** directory

* Unit tests are **source** files

    - **Manually** modified
    - Under **configuration control**
    - **Persistent**
    - Should be put **elsewhere**

--------------------------
Where To Put Unit Tests?
--------------------------

* **Not** in the object directory
* **Separate** from application code
* Use :toolname:`GNATtest` switches
* Having all tests in :filename:`source-dir/` is an issue

    - **All** directories rooted at :filename:`source-dir`
    - Treated as **application** source directories
    - Tests would be treated as part of the **application**

----------------------------------------
Switches for Tests Directory Structure
----------------------------------------

* Three **mutually exclusive** commands

:command:`--tests-dir=<tests_directory>`

    - **All** generated test packages are in :filename:`<tests_directory>/`
    - May be relative to the object dir

:command:`--tests-root=<tests_root>`

    - For each tested package :filename:`<package.ads>`

        * Associated test packages are in :filename:`<tests_root>/<package>/`
        * Test packages hierarchy is followed in :filename:`<tests_root>/`

    - :filename:`<tests_root>` may be relative to the object dir

:command:`--subdirs=<dirname>`

    - For each tested package :filename:`<package_src>/<package.ads>`

        + Associated test packages are in :filename:`<package_src>/<dirname>/`

----------------------------
:code:`--test-dir` Example
----------------------------

:command:`gnattest -P simple --test-dir=../unit_tests`

* **All** test packages are directly in :filename:`root/unit_tests/`

.. image:: images/gnattest/test-dir_switch.jpg

------------------------------
:code:`--tests-root` Example
------------------------------

:command:`gnattest -P simple --tests-root=../unit_tests`

* Associated test packages are in :filename:`root/unit_tests/<package>/`
* Internal test packages hierarchy is **replicated**

.. image:: images/gnattest/tests-root_switch.jpg

---------------------------
:code:`--subdirs ` Example
---------------------------

:command:`gnattest -P simple --subdirs=unit_tests`

* Test packages are in :filename:`root/src/package/.../unit_tests/`
* Warning: recursive :ada:`Source_Dirs` globs (eg :filename:`src/**`) will cause errors

.. image:: images/gnattest/subdir_switch.jpg

----------------------
Project File Support
----------------------

* :ada:`package GNATtest`
* **Some** switches have a specific attribute

    - :ada:`Harness_Dir`
    - :ada:`Tests_Dir`
    - :ada:`Tests_Root`
    - :ada:`Subdir`
    - :ada:`Additional_Tests`
    - :ada:`Skeletons_Default`

* **Other** switches in list attribute :ada:`GNATtest_Switches`

.. include:: examples/simple_stubbing/extracts/ops.package_gnattest.gpr
        :code: Ada

-------------------------------------
Fundamental Concept of the Approach
-------------------------------------

* Code **under test** in a package
* Tests are **children** of the package under test

    - **No alteration** to the code under test
    - Access to private part for **white-box** tests

* Package under test:

   .. code:: Ada

      package Parent is
         ...
      private
         ...
      end Parent;

* Test declarations:

   .. code:: Ada

      package Parent.Child is
         ...
      private
         ...
      end Parent.Child;

* Test bodies

   .. code:: Ada

      package body Parent.Child is
         ...
      end Parent.Child;

---------------------------------------
Reminder on Child Packages Visibility
---------------------------------------

* Parent

   .. include:: examples/stacks/extracts/integer_stacks.type_decl.ads
      :code: Ada

* Child

   .. include:: examples/stacks/src/integer_stacks-utils.adb
      :code: Ada

-----------------------------
Test Skeleton Naming Scheme
-----------------------------

* Given :ada:`package Pkg` under test
* Generates :ada:`package Pkg.Test_Data`

    - With :ada:`procedure Set_Up`, :ada:`procedure Tear_Down`

* :ada:`package Pkg.Test_Data.Tests` with test routines

    - One for each subprogram
    - Generated test routines get **unique** numeric suffixes

* Warning: Conflict with any existing :ada:`package Test_Data`

.. image:: images/gnattest/naming_scheme.jpg

------------------------------------------
Building & Executing the Generated Tests
------------------------------------------

* Via the (re)generated **harness** code
* Building

    - Entry point is **generated** :filename:`test_driver.gpr`
    - :command:`gprbuild -P<harness-dir>/test_driver`

* Executing

    - Main program is :filename:`test_runner`
    - :command:`<harness-dir>/test_runner`

* You may need to specify scenario variables' values

    - Else uses the :toolname:`AUnit` defaults
    - :code:`-Xvariable=value`

------------------------------------------
In Practice: Unimplemented Tests Results
------------------------------------------

* Generated test drivers report a **default** result
* As **failed**

    - Useful to see which tests are still unimplemented
    - **Default** behavior

* As **passed**

    - To sort those unimplemented from those **actually** failing

* Controlled by user

    - Switch :code:`--skeleton-default=value`
    - :ada:`package GNATtest`'s attribute :code:`Skeleton_Default`
    - Value is either :code:`fail` or :code:`pass` (lowercase)

----------------------
The "Simple" Example
----------------------

* Included in the GNAT installation examples
* Package Specification

.. include:: examples/gnat_simple/extracts/simple.ads
   :code: Ada

* Package Body

.. include:: examples/gnat_simple/extracts/simple.adb
   :code: Ada

------------------------------------
Generated Child Package Test_Data
------------------------------------

.. container:: latex_environment small

 .. include:: examples/gnat_simple/obj/gnattest/tests/simple-test_data.ads
   :code: Ada

-------------------------------------
Test Case Declaration, As Generated
-------------------------------------

* Unique names **guaranteed** by code generator

    - Handles **overloading**, if any, in application code

.. container:: latex_environment small

 .. include:: examples/gnat_simple/obj/gnattest/tests/simple-test_data-tests.ads
   :code: Ada

-----------------------------
Assertion Facility Provided
-----------------------------

* A :ada:`procedure` rather than a :ada:`pragma`
* Exported from :ada:`AUnit.Assertions`

    :ada:`AUnit.Assertions.Assert (boolean-expression, message);`

    - :code:`boolean-expression` :math:`\rightarrow` Assert this proposition
    - :code:`message` :math:`\rightarrow` Message to display when assertion **fails**

.. code:: Ada

   AUnit.Assertions.Assert (Head = null,
                            "Head is not null initially.");

------------------------------
Test Case Body, As Generated
------------------------------

.. include:: examples/gnat_simple/extracts/simple-test_data-tests.before.adb
   :code: Ada

-------------------------
Modified Test Case Body
-------------------------

.. include:: examples/gnat_simple/extracts/simple-test_data-tests.after.adb
   :code: Ada

--------------------------------
Using the Package Private Part
--------------------------------

* Some implementation **details** are needed by the test code

    - **Type** declarations
    - **Subprogram** declarations
    - Et cetera

* Declare them as :ada:`private`
* They will be visible to test code
* They will **remain hidden** from client code

    - Good software engineering

-----------------
Support for OOP
-----------------

* Tests for tagged types are automatically **inherited**
* Verifies **Local Type Consistency**

    - Relaxed form of *Liskov Substitutability Principle* (LSP)
    - Ensures that each class pass the tests of its **parent** class
    - See DO-178C supplement on Object-Oriented Technology and Related Techniques (DO-332)

* Inherited tests can be **overridden** in subclasses

-----------------------------------
Test Inheritance for Tagged Types
-----------------------------------

.. image:: images/gnattest/test_inheritance_for_tagged_types.jpg

-----------------------------------------
Liskov Substitutability Principle (LSP)
-----------------------------------------

.. code:: Ada

    type Child is new Root with ...

* **Any** :ada:`R : Root` in code can be substituted by a :ada:`C : Child`

    - Without causing any **type error**
    - Without causing any **dynamic check** failure
    - Any code, including **tests**

* Essential property for **dispatching**, especially dynamically
* Allow for specific subclass **independence**

    - Data structures
    - Algorithms

* Very **useful** in OOP context

    - **Isolates** the effects of change

--------------------------------------
Subclass-Independent Data Structures
--------------------------------------

      .. code:: Ada

         package Robot is
            type Instruction is tagged private;
            procedure Respond (To : Instruction);
            ...
         end Robot;

      .. code:: Ada

         type Any_Instruction is access Robot.Instruction'Class;

         type Node;
         type List is access Node;
         type Node is record
            Command : Any_Instruction;
            Next    : List;
         end record;

.. columns::

    .. column::

        .. image:: images/gnattest/data_structure_hierarchy.jpg
           :width: 120%

    .. column::

        .. image:: images/gnattest/command_sequence_list.png
           :width: 180%

.

---------------------------------
Subclass-Independent Algorithms
---------------------------------

* **Transparently** invoke subclass-specific overridings

    - Using dynamic dispatching

.. code:: Ada

   procedure Perform (Commands : in List) is
      Ptr : List;
   begin
      Ptr := Commands;
      while Ptr /= null loop
         Ptr.Command.Respond;  -- dynamic dispatching
         Ptr := Ptr.Next;
      end loop;
   end Perform;

.. image:: images/gnattest/command_sequence_list.png

-----------------------------------------
Recap: Preconditions and Postconditions
-----------------------------------------

* Specifies subprogram obligations
* **Precondition** :math:`\rightarrow` Assertion expected to hold **before** the call

    - Obligation for the **caller**
    - Guarantee to the **implementation**

* **Postcondition** :math:`\rightarrow` Assertion expected to hold **after** the call

    - Obligation for the **implementation**
    - Guarantee to the **caller**

.. code:: Ada

   procedure Push (This : in out Stack;  Value : Content) with
      Pre  => not Full (This),
      Post => not Empty (This) and Top (This) = Value;
   ...
   function Top (This : Stack) return Content;
   function Full (This : Stack) return Boolean;

---------------------------------------
The Contractor-Subcontractor Metaphor
---------------------------------------

* Inheritance with **dynamic** dispatching

    - Clients may be using a supplier **subclass**
    - With **no** knowledge of it

* Supplier subclasses are **subcontractors**
* Can redefine the preconditions
* Can redefine the postconditions

-------------------------
Global Type Consistency
-------------------------

* A subcontractor will still accept the **same input**

    - But can accept **more** input types
    - **Never** less

* So preconditions must be same, or **weaker**

    - **No** demands beyond those of superclasses

* It will do the **same job**

    - Or **more** operations
    - **Never** less

* So postconditions must be same, or **stronger**

    - Guarantees at least **as much** as the superclasses

-----------------------------------
Verifying Global Type Consistency
-----------------------------------

* For a given derived type, run **all tests** from all parent types

    - Verifies **no stronger** preconditions
    - Verifies **no weaker** postconditions

* Requires switch :command:`--validate-type-extensions`
* Find tests that **would** otherwise pass

    - When applied to the type **defining** them

----------------------------
Support for External Tests
----------------------------

* eg. :toolname:`AUnit` tests created manually

    + For project :filename:`simple.gpr`
    + With existing tests in :filename:`additional/external.gpr`

* Use switch :command:`--harness-only`

    - **Only** generate the harness
    - Do **not** generate or update the drivers

:command:`gnattest -P additional/external.gpr --harness-only`
   :command:`--harness-dir=external`

* Generate the tests with :command:`--additional-tests=<project-file>`

    - Sources in :filename:`<project-file>.gpr` are considered manual tests

        + And are added to the test suite

:command:`gnattest -P simple.gpr`
   :command:`--additional-tests=additional/external.gpr`
   :command:`--harness-dir=external`

----------
Stubbing
----------

* Test a :ada:`package` in isolation from its dependencies
* :command:`--stub`
* :command:`-r` for recursivity

.. columns::

  .. column::

   * Original hierarchy

       .. image:: images/gnattest/stub_before.jpg

  .. column::

   * Stubbing :ada:`Input`

       .. image:: images/gnattest/stub_level1.jpg

   * Stubbing :ada:`Console`

       .. image:: images/gnattest/stub_level2.jpg

.

-------------------------------------------------------
Generating Tests with :toolname:`GNAT Studio`: - Menu
-------------------------------------------------------

.. image:: images/gnattest/invoke_gnattest.jpg

----------------------------------------------------------------
Generating Tests with :toolname:`GNAT Studio`: - Configuration
----------------------------------------------------------------

.. image:: images/gnattest/invoke_gnattest_dialog.jpg

--------------------------------------------
Running Tests with :toolname:`GNAT Studio`
--------------------------------------------

* Test generator setup menu will automatically switch to the test **harness** project
* Just **build and run** as any other project

    - Invocation dialog allows overriding **switches**

.. image:: images/gnattest/run_gnattest.jpg

-------------------------------------------
Test Results with :toolname:`GNAT Studio`
-------------------------------------------

.. columns::

  .. column::

    * Failed

    .. image:: images/gnattest/test_fail.png

  .. column::

    * Passed

    .. image:: images/gnattest/test_pass.png

* Source **under test** is automatically opened
* Can access failed **test** source from the **Locations** view

-----------------------------------------------------
Exiting :toolname:`GNAT Studio` Testing Perspective
-----------------------------------------------------

* To return to the development project

.. image:: images/gnattest/exit_gnattest.jpg

-------------------------------------------
Non-Native Platforms & Runtime Libraries
-------------------------------------------

* May support only a **subset** of full Ada

    - Memory allocation, exceptions, etc.

* Need to **adapt** test generation tests to those limitations
* Can be requested via :command:`-X` switch

    - :command:`-Xname=value`

* Defined scenario variable names:

    - :command:`PLATFORM`
    - :command:`RUNTIME`

* Supported values depend on licenses

---------------------------------
Example: LynxOS-178 for PowerPC
---------------------------------

* A cross-development platform

    - Hence we use the cross-development version of the tool
    - No need to specify PLATFORM

* Available run-time libraries

    - **pthread** (full Ada)
    - **ravenscar-cert**
    - **cert**
    - **zfp**

:command:`powerpc-xcoff-lynxos178-gnattest -P simple.gpr`
:command:`\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ -XRUNTIME=zfp`

--------------
Getting Help
--------------

* Invoke gnattest with :command:`--help` switch
* See the GNAT Pro User Guide

    - Section 27 "Creating Unit Tests with gnattest"
    - Available from within :toolname:`GNAT Studio`
    - Available under your GNAT Pro installation directory tree

        + In a subdir corresponding to the file format
        + File name is :filename:`gnat_ugn.[pdf | html | txt | info]`
        + Example: :filename:`/usr/gnat-21.2/share/doc/gnat/pdf/gnat_ugn.pdf`
        + Where :filename:`/usr/gnat-21.2/share/doc/gnat` is the default path

---------------------------------------------
Getting Help Within :toolname:`GNAT Studio`
---------------------------------------------

.. image:: images/gnattest/help_from_menu.jpg

----------------------------------
Currently Unsupported Constructs
----------------------------------

* Tests for protected subprograms and entries
* Generic units

    - But can test concrete instances

* See the latest GNAT Pro User Guide for status

---------------
DO-178C Ready
---------------

* Natural path from DO-178 low level requirements to structural coverage

.. image:: images/gnattest/do178c_ready.jpg

* Substitution verification implemented, to support OOP supplement of DO-178C

------------------
GNATtest Summary
------------------

* **Automatically** creates and updates code

    - Harness
    - Unit test skeletons

* Developers can **focus** on actual test cases
* Adapted high levels of **reliability**, **safety**, and **security**

    - Simplifies effort required to implement **required** test procedures
    - Can use :toolname:`GNATcoverage` to verify test completeness

* Fully integrated into :toolname:`GNAT Studio`
* Supports large choice of platforms

    - Native
    - Cross
    - High-integrity
