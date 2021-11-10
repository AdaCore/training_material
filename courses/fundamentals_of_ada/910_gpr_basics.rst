************
GPR Basics
************

.. include:: support_files/symbols.rst

==============
Introduction
==============

----------------------------------
Origins and Purposes of Projects
----------------------------------

* Need for **flexibility**

   - Managing large applications is **hard**
   - A common tool benefits many AdaCore customers

* GNAT compilation model

   - Compile when :ada:`with`'ed (except main subprograms)

       + Recursively

   - Compiler needs to know **where** to find the source files

* :toolname:`GNAT Studio` (IDE) need to find

   - **Source** files
   - **Object** files
   - **Linker** information
   - Project configuration (**scenario**, **sub-projects**...)

* Various Tools

   - Metrics, Documentation generator, etc...
   - Benefit from knowledge of the application **structure**

--------------------
Projects Hierarchy
--------------------

.. container:: columns

 .. container:: column

    * Support **incremental, modular** project definition
    * Can **import** sub-projects

       - **Hierarchical** subsystems
       - Build decisions **deferred** to the subsystem level

    * Can **extend** parent projects

       - **Inherit** source files
       - Optional **overriding**

 .. container:: column

    .. image:: images/connected_cubes.png

--------------------
GNAT Project Files
--------------------

* Text files with **Ada-like** syntax
* Also known as **gpr files** due to file extension
* Integrated into **command-line** tools

   - Specified via the **-P project-file-name** switch

* Integrated into the **IDEs**

   - A fundamental part
   - **Automatic** update graphically
   - Optional **automatic generation**

* **Should** be put under configuration management

-------------------------
Configurable Properties
-------------------------

* **Source** directories and specific files' names
* **Output** directory for object modules and .ali files
* Target directory for executable programs
* **Switch** settings for supported tools
* Source files for **main** subprogram(s) to be built
* Source programming **languages**

   - Ada / C / C++ are preconfigured

* Source file **naming conventions** (:code:`ads` vs :code:`ad0`)
* **Large** set of specific configurations

--------------------------
The Minimal Project File
--------------------------

* No main
* Source are in gpr file's directory
* Output files are put in gpr file's directory

.. code:: Ada

   project My_Project is

   end My_Project;

-----------------------------------
Comparison with Makefiles
-----------------------------------

* A Makefile performs actions (indirectly)

   - **Dependency** tree declaration
   - **Procedural** build resolution
   - **Adaptable** to large set of conditions

* A project file describes an Ada project

   - **Implicit** dependency tree
   - **Declarative** build resolution
   - **Limited** to supported build process

* Both approaches are complementary
* Usually a **top-level** Makefile

.. code:: console

    build-gpr:
       gprbuild -P <project-file> ...

    all: pre-build build-gpr post-build

--------------------------
Advantages over Makefile
--------------------------

* Syntax **close** to Ada

   - Clearer
   - No exotic operators
   - No project-specific idioms or syntax

* No issue with **dependency** declaration

   - Rebuilds **as needed**
   - **Order** of dependencies guaranteed
   - Dependency of sources over **scenario** variables
   - Sub-projects

* Simpler, more **deterministic**
* More **performant**

===============================
Configuring Project Properties
===============================

------------------------------
Variables Types Introduction
------------------------------

* Strings

   .. code:: Ada

      "main.adb"

* Lists of strings

   .. code:: Ada

      ("-v", "-gnatv")

* Associative arrays

   - Array-like syntax
   - **Implicitely** system-defined
   - Mapped to **either** single string or list of strings
   - May hide a **subprogram call**

   .. code:: Ada

      for <name> (<string-index>) use <list-of_strings>;

-----------
Variables
-----------

* Typed

   - **Restricted** to a set of String values
   - **Can** be user-defined

* Untyped

   - Strings or lists
   - **Can** be user-defined

* Attributes

   - Any type, including **arrays**
   - **Cannot** be user-defined

------------------------
Variables Declarations
------------------------

* Typed variables limited to a set of values

   - Values as **String**, unlike Ada
   - **Case sensitive**, unlike Ada

* Typed variables are declared **once** per scope

   - Once at :ada:`project` level
   - Once within any :ada:`package` block
   - Essentially **read-only** constants

      + Especially nice for **external** inputs

* Untyped variables may be **re-declared**

   - **No** previous declaration required

----------------------
Variables Assignment
----------------------

* User-defined use :ada:`:=`

    + Typed or untyped

.. code:: Ada

   project Build is
      type Targets is ("release", "test");
      -- typed variable
      Target : Targets := external ("target", "test");
      -- untyped string variable
      Var := "foo";
      -- untyped string list variable
      Var2 := ("-gnato", "-gnata");
      ...
   end Build;

* Attributes use :ada:`for`

   + Scalar, lists
   + Associative arrays elements

.. code:: Ada

   project Build is
      for Object_Dir use "obj";
      for Main use ("main1.adb", "main2.adb");
      for Runtime ("Ada") use ...

--------------------------------
"Packages" Correspond to Tools
--------------------------------

* :ada:`package Builder`

  :command:`gprbuild`

* :ada:`package Compiler`

  :command:`gcc`

* :ada:`package Linker`

  :command:`gnatlink`

* :ada:`package Binder`

  :command:`gnatbind`

* Others...
* Names and content defined by **vendor**

   - Not by users

-----------------------
Setting Tool Switches
-----------------------

* Command-line **options**
* May be specified to apply **by default**

   - Array :ada:`Default_Switches`
   - Indexed by **language**

   .. code:: Ada

      package Compiler is
         for Default_Switches ("Ada") use ("-gnaty", "-v");
      end Compiler;

* May be overloaded on a **per-unit** basis

   - Array :ada:`Switches`
   - Indexed by **unit name**

   .. code:: Ada

      package Builder is
         for Switches ("main1.adb") use ("-O2");
         for Switches ("main2.adb") use ("-g");
      end Builder;

-------------------------------
Specifying Main Subprogram(s)
-------------------------------

* Optional

   - Can be specified with switch :command:`gprbuild gpr-file [switches] main-file`

* Project-level setting
* Can specify several main files

   - One executable per main specified

.. code:: Ada

   project Foo is
      --  Generates executables `bar` and `baz`
      for Main use ("bar.adb", "baz.adb");
   end Foo;

=========================
Extending and Importing
=========================

-----------
Extending
-----------

* Sort of **template** project
* Use **modified** versions of the source files
* **No** change to the original sources
* Extension-specific attributes

   - :ada:`Excluded_Source_Files`, :ada:`Excluded_Source_List_File`

* :ada:`extends` keyword

.. code:: Ada

   project Work extends "../bld/build.gpr" is

      for Source_Files use ("pack.ads");
      --  New spec of Pkg does not need a completion
      for Excluded_Source_Files use ("pack.adb");

   end Work;

-----------
Importing
-----------

* Use the project as a **sub-project**
* :ada:`with` keyword

.. code:: Ada

   with "my_driver/my_driver.gpr";
   project B is

   end B;

* Access the sub-project **attributes** with :code:`<subp>'<attr>`
* Access the sub-project **variables** with :code:`<subp>.<var>`

.. code:: Ada

   for Target use My_Driver'Target;

   case My_Driver.Var is
   ...

-------------------------
Searching for GPR Files
-------------------------

* :toolname:`GPRbuild` search the .gpr files using
* **Absolute** path
* **Relative** path

   - Relative to current project file
   - Relative to any path in :command:`GPR_PROJECT_PATH_FILE`

      + Several other environment variables used otherwise

========================
Specifying Directories
========================

------------------------
Specifying Directories
------------------------

* **Any** number of source directories

   - Source directories contain **source files**
   - Project file's directory **by default**

* A **single** object directory

   - Contains **object** files
   - Contains **all generated files**
   - Project file's directory **by default**

* A **single** executables directory

   - Contains the **executables**
   - By default, it is the **object directory**

--------------------
Source Directories
--------------------

* One or more in any project file
* Project file's directory **by default**
* Can be **re-defined**

   .. code:: Ada

      for Source_Dirs use ("mains", "drivers");
      ...
      for Source_Dirs use ("mains", "mock_drivers");

* Can specify that none are present

   .. code:: Ada

      for Source_Dirs use ();

--------------
Source Files
--------------

* Can specify source files **by name**

   .. code:: Ada

      for Source_Files use ("main.adb", "pack1.ads", "pack2.adb");

* Can specify that **none** are present

   .. code:: Ada

      for Source_Files use ();

* Can specify an **external file** containing source names

   .. code:: Ada

      for Source_List_File use "source_list.txt";

------------------
Object Directory
------------------

* Specifies the location for tools' output

   - Such as "ali" files and object files
   - For the project's immediate sources

   .. code:: Ada

      project Release is
        for Object_Dir use "release";
        ...
      end Release;

* Only **one** per project
* For extension, the **child**'s object directory is used for sources

    - Except those already compiled in the parent

----------------------
Executable Directory
----------------------

* Specifies the location for **executable** output

   .. code:: Ada

      project Release is
        for Exec_Dir use "executables";
        ...
      end Release;

* Default is **same** directory as object files
* Only **one** per project

=======================
Naming Considerations
=======================

----------------------------
Source File Naming Schemes
----------------------------

* Allow **arbitrary** naming conventions

   - Not limited to GNAT default convention

* May be applied to **all** source files in a project

   - Specified in :ada:`package Naming`

* May be applied to specific files in a project

   - Individual **attribute** specifications

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

------------------------------------
Individual (Arbitrary) File Naming
------------------------------------

* Uses arrays :ada:`Spec` and :ada:`Body` to specify file names

   - Indexed by the unit name
   - Value is a String

      + Case sensitivity depends on **host** file system

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

* Import **environment** variables and command-line **switches**

  .. code:: Ada

     external (name [, default])

* Value of :ada:`name` as supplied

   - On the command line

     :command:`gprbuild -Xname=value`

   - Else as environment variable

     :command:`name=value gprbuild`

   - If none found, return :ada:`default`
   - If no :ada:`default`, return :ada:`""`

-------------------
Case Construction
-------------------

* Similar to Ada's :ada:`case` statement
* Conditional execution
* Depends on :code:`<variable>`'s String value

.. code:: Ada

  case <variable> is
     when "Choice" =>
     ...
     when "Other_Choice" | "Third_Choice" =>
     ...
  end case;

* Empty declaration can be :ada:`null`
* If variable is typed, all values must be covered **exactly once**

    - Can use :ada:`when others`

.. code:: Ada

  case OS is
     when "Unix" =>
     ...
     when others =>
        null;
  end case;

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

-------------------
Cross-Compilation
-------------------

* Target

   - Compiler target
   - Target that the compiled code **runs on**
   - Default is **host machine**

* RTS

   - Runtime used
   - Default depends on &&toolchain**

* Specified via **switches**

  :command:`gprbuild -target=<target> -RTS=<rts>`

* Can also be specified with attributes

.. code:: Ada

  for Target use "arm-eabi";
  for Runtime ("Ada") use "ravenscar-sfp-stm32f429disco";

* Notice runtime is actually a pre-built binary for the exact target

===========
GPRconfig
===========

--------------------
Configuration File
--------------------

* Describes **languages** and **toolchains** used
* Extension :command:`.cgpr`
* Typically created **automatically** by :toolname:`GPRbuild` based on

   - Languages defined in project files
   - Compilers in :command:`PATH`

- Create it **explicitely** with

  :command:`gprbuild -autoconf=<file.cgpr>`

-----------------------------
Default Configuration Files
-----------------------------

* :toolname:`GPRbuild` searches for configuration file

   - ``<target>-<rts>.cgpr``
   - ``<target>.cgpr``
   - ``<rts>.cgpr``
   - ``default.cgpr``

* **RTS**: RunTime
* Default directory is :code:`<gprbuild_install>/share/gpr`
* Check environment variable :command:`GPR_CONFIG` for a valid file

   * Must contain an **absolute** path
   * Can be cgpr **file** directly
   * Or **default** directory to search from

------------------------------
Creating Configuration Files
------------------------------

* Preferable (and often necessary) to generate your own when

   - Wrong autoconfig
   - Cross compilers
   - Specific Ada runtime
   - Compilers **not in** :command:`PATH`

      - Or not first in :command:`PATH`

* Default method

   - Interactive mode lists all known compilers

     :code:`gprconfig --target=ppc-elf`

      + List cross compilers for target :code:`ppc-elf`

   - Select a compiler for each of language
   - Compatible compilers for other languages are proposed

-------------------------
Examples of "gprconfig"
-------------------------

* Interactive (expects user inputs)

   :command:`gprconfig`

   - File will be generated in GPRbuild's default location, (./default.cgpr)

   :command:`gprconfig -o path/my_config.cgpr`

   - File stored in :command:`path/my_config.cgpr`

* Automatic (batch) with :code:`gprconfig --batch`

   :code:`gprconfig --config=Ada --config=C --batch`

   - Generates at **default** location
   - Using **first** native Ada and C compilers on path.

   :code:`gprconfig --target=leon-elf --config=Ada,,hi --config=C --batch -o x.cgpr`

   - Generates configuration file :code:`x.cgpr`
   - For cross-compiling on LEON with Ada run-time :code:`hi`
   - Using Ada and C

====================
GPRconfig Switches
====================

------------------------
Command line arguments
------------------------

* :code:`--target=<platform>`

   - Indicates cross-compiler target
   - Example: :code:`--target=ppc-elf`
   - Special target "all" to display **all targets** on path

* :code:`--show-targets`

   - List all supported :code:`--target`

* :code:`--config=language[,version[,runtime[,path[,name]]]]`

   - Preselect **one or more** compilers directly from the command line
   - Optional arguments will be computed **automatically**

------------------------------------
Command line arguments (continued)
------------------------------------

* :code:`--batch`

   - **Automatically** select **first** compiler matching
   - Guided by :code:`-config` switches
   - Never interactive

* :code:`-o <file>`

   - Name of generated **configuration file**
   - Default is a file in :toolname:`GPRbuild`'s installation directory

* :code:`--db directory, --db-**`

   - Add a directory to parse for configuration files

* :code:`-h`

   - Generates help message
   - Lists all :toolname:`GPRconfig` switches with their default values

=========
Summary
=========

------------------------------
GNAT Project Manager Summary
------------------------------

* Supports **hierarchical**, localized build decisions
* IDEs provide **direct support**
* See the **GNAT Pro User's Guide** for further functionality and capabilities
* This set of slides is far from being exhaustive
