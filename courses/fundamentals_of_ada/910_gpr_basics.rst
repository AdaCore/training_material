
********************
Annex - GPR Basics
********************

==============
Introduction
==============

----------------------------------
Origins and Purposes of Projects
----------------------------------

* The need for flexibility

   - Managing huge applications is a difficult task; many AdaCore customers benefit from a tool to help them

* The GNAT compilation model

   - Compiler needs to know where to find the Ada files imported by the Ada unit which is being compiled

* The IDEs

   - AdaCore IDEs need to know where to find the source files, and also the object files

* The Tools (metrics, documentation generator, etc)

   - AdaCore Tools benefit from having knowledge of the application structure

---------------------------------
Subsystems of Subsystems of ...
---------------------------------

.. container:: columns

 .. container:: column
  
    * Projects support incremental, modular project definition

       - Projects can import other projects containing needed files 

       - Child projects can extend parent projects, inheriting source files and optionally overriding any of them

    * Facilitates the structuring of large development efforts into hierarchical subsystems

       - With build decisions deferred to the subsystem level

 .. container:: column
  
    .. image:: ../../images/connected_cubes.png

--------------------
GNAT Project Files
--------------------

* Text files with Ada-like syntax
* Also known as "gpr files" due to file extension
* Integrated into command-line tools

   - Specified via the **-P project-file-name** switch

* Integrated into the IDEs

   - A fundamental part
   - Automatically generated if desired

* Should be under configuration management

-------------------------
Configurable Properties
-------------------------

* Source directories and specific files' names
* Output directory for object modules and .ali files
* Target directory for executable programs
* Switch settings for project-enabled tools

* Source files for main subprogram(s) to be built 
* Source programming languages 

   - Ada / C / C++ are preconfigured

* Source file naming conventions
* et cetera

--------------------------
The Minimal Project File
--------------------------

.. code:: Ada

   project My_Project is
   
   end My_Project;
 
-----------------------------------
About Project Files and Makefiles
-----------------------------------

* A Makefile performs actions (indirectly)
* A project file describes a project
* Command lines using project files fit naturally in the Makefile paradigm

.. code:: console

   gprbuild -P <project-file> ...
 
===============================
Configuring Project Properties
===============================

------------------------------
Property Values Introduction
------------------------------

* Strings
* Lists of strings

   .. code:: Ada

      ("-v", "-gnatv")
 
* Associative arrays

   - Like functions that map input string to either single string or list of strings

   .. code:: Ada

      for <name> (<string-index>) use <list-of_strings>;
 
-----------
Variables
-----------

* "Typed" - a set of possible string values
* "Untyped" - unspecified set of values

   - Strings and lists

.. code:: Ada

   project Build is
      type Targets is ("release", "test");
      -- typed variable
      Target : Targets := external("target", "test");
      -- untyped string variable
      Var := "foo";
      -- untyped string list variable
      Var2 := ("-gnato", "-gnata");
      ...
   end Build;
 
--------------------------------
Typed Versus Untyped Variables
--------------------------------

* Typed variables have only listed values possible 

   - Case sensitive, unlike Ada

* Typed variables are declared once per scope

   - Once at project level 

   - Once within any package

   - Essentially read-only constants 

      + Especially nice for external inputs

* Untyped variables may be "declared" many times

   - No previous declaration required

--------------------------------
"Packages" Correspond to Tools
--------------------------------

* **Builder**

   - *gprbuild*

* **Compiler**

   - *gcc*

* **Linker**

   - *gnatlink*

* **Binder**

   - *gnatbind*

* Others...
* Allowable names and content defined by vendor

   - Not by users

-----------------------
Setting Tool Switches
-----------------------

* May be specified to apply by default

   .. code:: Ada

      package Compiler is
         for Default_Switches ("Ada") use ("-gnaty", "-v"); 
      end Compiler; 
 
* May be specified on a per-unit basis

   - Associative array "Switches" indexed by unit name

   .. code:: Ada

      package Builder is
         for Switches ("main1.adb") use ("-O2");
         for Switches ("main2.adb") use ("-g"); 
      end Builder;
 
-------------------------------
Specifying Main Subprogram(s)
-------------------------------

* Optional, otherwise requires specification on command line to build
* Can have more than one file named
* A project-level setting

.. code:: Ada

   project Foo is
      for Main use ("bar.adb", "baz.adb"); 
   end Foo; 
 
========================
Specifying Directories
========================

------------------------
Specifying Directories
------------------------

* Any number of Source Directories

   - Source Directories contain Source Files
   - By default, the directory that contains the project file
   - It is possible to create a project with no Source Directory

* One Object Directory

   - Contains object files and any files generated by the tools
   - By default, the directory that contains the Project File

* One Executables Directory

   - Contains the executables
   - By default, it is same as Object Directory

--------------------
Source Directories
--------------------

* One or more in any project file
* Default is same directory as project file
* Can specify additional / other directories

   .. code:: Ada

      for Source_Dirs use ("mains", "drivers"); 
 
* Can specify that none are present

   .. code:: Ada

      for Source_Dirs use (); 
 
--------------
Source Files
--------------

* Must be at least one "immediate" source file

   - In one of the source directories of the project file
   - Unless explicitly specifies none present

   .. code:: Ada

      for Source_Files use ();
 
* Can specify source files by name

   .. code:: Ada

      for Source_Files use ("main.adb","pack1.ads","pack2.adb");
 
* Can specify an external file containing source names

   .. code:: Ada

      for Source_List_File use "source_list.txt";
 
------------------
Object Directory
------------------

* Specifies the location for compiler's output 

   - Such as "ali" files and object files
   - For the project's immediate sources

   .. code:: Ada

      project Release is
        for Object_Dir use "release";
        ...
      end Release;
 
* Only one per project

   - When extending a parent project the child's object directory is used for any inherited sources not already compiled in the parent

----------------------
Executable Directory
----------------------

* Specifies the location for executable image 

   .. code:: Ada

      project Release is
        for Exec_Dir use "executables";
        ...
      end Release; 
 
* Default is same directory as object files
* Only one per project

=======================
Naming Considerations
=======================

----------------------------
Source File Naming Schemes
----------------------------

* Allow arbitrary naming conventions

   - Other than GNAT default convention

* May be applied to all source files in a project

   - Specified in a package named "Naming"

* May be applied to specific files in a project

   - Individual attribute specifications

-------------------------------------
Foreign Default File Naming Example
-------------------------------------

.. code:: Ada

   project Rational is
     ...
     package Naming is
       for Casing use "lowercase";
       for Dot_Replacement use ".";
       for Spec_Suffix ("Ada")  use ".1.ada";
       for Body_Suffix ("Ada") use ".2.ada";
     end Naming;
     ...
   end Rational;
 
----------------------------------
GNAT Default File Naming Example
----------------------------------

.. code:: Ada

   project GNAT is
     ...
     package Naming is
       for Casing use "lowercase";
       for Dot_Replacement use "-";
       for Spec_Suffix ("Ada")  use ".ads";
       for Body_Suffix ("Ada") use ".adb";
     end Naming;
     ...
   end GNAT;
 
------------------------------------
Individual (Arbitrary) File Naming
------------------------------------

* Uses associative arrays to specify file names

   - Index is a string containing the unit name
   - Value is a string

      + Case sensitivity depends on host file system

* Has distinct attributes for specs and bodies

.. code:: Ada

   for Spec ("MyPack.MyChild") - unit name
      use "MMS1AF32"; -- base file name
   for Body ("MyPack.MyChild") - unit name
      use "MMS1AF33"; -- base file name
 
====================
Adding Flexibility
====================

----------------------------------------
Projects for Different Switch Settings
----------------------------------------

.. code:: Ada
    
   project Debug is 
     for Object_Dir use "debug"; 
     package Builder is
       for Default_Switches ("Ada")
         use ("-g"); 
     end Builder; 
     package Compiler is
       for Default_Switches ("Ada") 
          use ("-fstack-check", "-gnata", "-gnato"); 
     end Compiler;
   end Debug;
     
   project Release is
     for Object_Dir use "release";
     package Compiler is 
       for Default_Switches ("Ada")
          use ("-O2"); 
     end Compiler;
   end Release;
 
-------------------------------------
External and Conditional References
-------------------------------------

* Allow project file content to depend on value of environment variables and command-line arguments
* Reference to external values is by function

   - **external( name [, default] )** returns value of name as supplied on the command line or as environment variable
   - If name is undefined, return default (if supplied) or ""

* Set via command line switch (for example)

.. code:: console

   gprbuild -P... -Xname=value  ...
   gprbuild -P/common/build.gpr -Xtarget=test  /common/main.adb
 
----------------------------------------
External/Conditional Reference Example
----------------------------------------

.. code:: Ada
    
   project Build is
      type Targets is ("release", "test");
      Target : Targets := external("target", "test");
      case Target is -- project attributes
         when "release" =>
            for Object_Dir use "release";
            for Exec_Dir use ".";
         when "test" =>
            for Object_Dir use "debug";
      end case;
     
      package Compiler is
         case Target is
            when "release" =>
               for Default_Switches ("Ada")
                  use ("-O2");
            when "test" =>
               for Default_Switches ("Ada")
                  use ("-g", "-fstack-check", "-gnata", "-gnato");
         end case;
      end Compiler;
          ...
   end Build;
     
===========
GPRCONFIG
===========

--------------------------------
Configuration File Description
--------------------------------

* Describes languages and toolchains used
* Typically created automatically by GPRbuild based on

   - Languages defined in GPR files
   - Compilers on path

-----------------------------
Default Configuration Files
-----------------------------

* GPRbuild searches for configuration file

   - Search default configuration file for file

      * ``<target>-<rts>.cgpr``
      * ``<target>.cgpr``
      * ``<rts>.cgpr``
      * ``default.cgpr``
      * *Target and RTS parameters are specified via -target and -RTS switches of gprbuild*
      * *Default directory is share/gpr in gprbuild installation directory*

   - Check environment variable :command:`GPR_CONFIG` for valid configuration file

      + Either absolute path name or base name for searching as above

   - If -autoconf specified, new configuration file is automatically generated

      + Based on specified target and languages specified in projects.

------------------------------
Creating Configuration Files
------------------------------

* Preferable (and often necessary) to generate your own when

   - Cross compilers

      + --target=

   - Specific Ada runtime

      + --RTS=

   - Compilers not in the path (or not first in the path)
   - autoconfiguration does not give the expected results.

* Default method

   - Simple interactive mode lists all known compilers for all known languages
   - Select a compiler for each of the languages
   - Compatible compilers for other languages are proposed.

-------------------------
Examples of "gprconfig"
-------------------------

* Interactive

   - :command:`gprconfig`

      + File will be generated in GPRbuild's default location, (./default.cgpr)

   - :command:`gprconfig -o path/my_config.cgpr`

      + File stored in :command:`path/my_config.cgpr`

   - :command:`gprconfig --target=ppc-elf`

      + Only relevant cross compilers for target ppc-elf will be proposed

* Automatic (batch)

   - :command:`gprconfig --config=Ada --config=C --batch`

      + Generates at default location using first native Ada and C compilers on path.

   - :command:`gprconfig --target=leon-elf --config=Ada,,hi --config=C --batch -o x.cgpr`

      + Generates configuration file named x.cgpr for cross-compiling Ada with a run-time called hi and using C for the LEON processor.

===================
GPRCONFIG Options
===================

------------------------
Command line arguments
------------------------

* *--target=platform*

   - Indicates target computer on which your application will run. 

   - Example: **--target=ppc-elf**
   - Special target "all" to display all targets on path
   - Default target is host machine

* *--show-targets*

   - List targets that are compatible with **--target**

* *--config=language[,version[,runtime[,path[,name]]]]*

   - Preselect one or more compilers directly from the command line
   - Optional arguments will be computed automatically

------------------------------------
Command line arguments (continued)
------------------------------------

* **--batch**

   - Automatically select first compiler matching each of the -config switches

      + Not interactive

* **-o file**

   - Specify name of generated configuration file that will be generated
   - If not specified, a default file is generated in installation directory of GPRbuild

* **--db directory, --db-**

   - Indicates another directory that should be parsed for GPRconfig's knowledge base

* **-h**

   - Generates help message listing all GPRconfig switches and their default values

=========
Summary
=========

------------------------------
GNAT Project Manager Summary
------------------------------

* Supports hierarchical, localized build decisions
* IDEs provide direct support
* See the GNAT Pro User's Guide for further functionality and capabilities
* We haven't covered everything by any means!
