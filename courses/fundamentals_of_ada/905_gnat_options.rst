**************
GNAT options
**************

.. |rightarrow| replace:: :math:`\rightarrow`

------------------------------------
Understanding the GNAT Build Steps
------------------------------------

* **gcc** is responsible to compile source files into object files

   - package `Compiler` of the gpr file

* **gnatbind** is responsible to schedule the elaboration of all units

   - package `Binder` of the gpr file

* **gnatlink** is responsible for linking the application into one executable / library

   - package `Linker` of the gpr file

* **gprbuild** is responsible of calling these tools

----------------------
Targets and Runtimes
----------------------

* Most tools have a "native" name (gcc, gnat, gnatcheck, etc.)
* The name of the tool for the target is "**target**-**toolname**"

   - powerpc-wrs-vxworksae-gcc
   - powerpc-wrs-vxworksae-gnatcheck

* Exceptions: gnatstack, gprbuild, gnatstudio
* The runtime is introduced with the **--RTS=** switch

   - powerpc-wrs-vxworksae-gcc --RTS=zfp

---------------------------
Some useful GNAT switches
---------------------------

.. container:: columns

 .. container:: column
  
    * **-O[0,1,2,3,s]**

       - Turns optimizations (0 = Minimal, 3 = Maximal, s = optimize for size)

    * **-g**

       - Turn on debug information

    * **-c**

       - Only compile (do not build)

    * **-gnatc**

       - Do only semantic analysis (do not generate code)

 .. container:: column
  
    * **-gnatn**

       - Activates `pragma Inline`

    * **-gnata**

       - Activates assertions

    * **-gnateE**

       - Extends messages produced for run-time checks

    * **-gnatp**

       - Removes run-time checks

-----------------
Validity checks
-----------------

* **-gnatV[x]** options add checks on values validity
* Checks if variables have an expected value in a lot of places
* Expensive test (this is why it's not added by default)
* `pragma Normalize_Scalar`

   - Makes sure that uninitialized variables are initialized with invalid default

---------------------
Data representation
---------------------

* **-gnatDG** produces *filename.dg* representing the intermediate representation 

* Code is expanded into simple structures and system calls
* Useful to understand the complexity of the Ada constructions
* Useful to identify check locations
* Integrated into GNAT Studio

-----------------------------
Intermediate representation
-----------------------------

.. container:: columns

 .. container:: column

   * ``-gnatR#`` displays representations of

      .. list-table::

         * - 0

           - None

         * - 1

           - Type

         * - 2

           - All

         * - 3

           - Variable

      * Helps optimizing data structures

 .. container:: column

   * ``-gnatR1``

      .. code:: Ada

         type Rec1 is record
            A : Boolean;
            B : Integer;
            C : Boolean;
         end record;
         for Rec1'Object_Size use 96;
         for Rec1'Value_Size use 72;
         for Rec1'Alignment use 4;
         for Rec1 use record
            A at 0 range  0 ..  7;
            B at 4 range  0 .. 31;
            C at 8 range  0 ..  7;
         end record;


------------------------------------
Intermediate representation (cont)
------------------------------------

.. container:: columns

 .. container:: column

   * ``-gnatR2``

      .. code:: Ada

         type Rec2 is record
            A : Boolean;
            C : Boolean;
            B : Integer;
         end record;
         for Rec2'Size use 64;
         for Rec2'Alignment use 4;
         for Rec2 use record
            A at 0 range  0 ..  7;
            C at 1 range  0 ..  7;
            B at 4 range  0 .. 31;
         end record;
 
 .. container:: column

   * ``-gnatR3``

      .. code:: Ada

         type Rec3 is record
            A : Boolean;
            B : Integer;
            C : Boolean;
         end record;
         pragma Pack (Rec3);
         for Rec3'Object_Size use 40;
         for Rec3'Value_Size use 34;
         for Rec3'Alignment use 1;
         for Rec3 use record
            A at 0 range  0 ..  0;
            B at 0 range  1 .. 32;
            C at 4 range  1 ..  1;
         end record;
 
----------
Inlining
----------

* Must be activated through **-gnatn**
* Subprograms are selected through `pragma Inline`
* Dependencies need visibility on the body (inlining works cross unit)
* **gnatcheck** can flag wrong (too complex) inlining

-----------------------
Some Additional Tools
-----------------------

* **gprclean**

   - Removes all compilation products (.o, .ali, .exe files)

* **gnatstub**

   - Generates a package body given a package declaration

* **gnatls**

   - Library browser

* **gnatprep**

   - Integrated preprocessor

* Many more dedicated tools for static and dynamic analysis of the program
