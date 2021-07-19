
***********
GNATstack
***********


-----------
GNATstack
-----------

+ A static analyzer for maximum stack requirements

  + Max per subprogram
  + Max per task

+ Conservative: indicates when it's not accurate
+ Sound: believable when it says it is accurate
+ Especially useful in a high-integrity context

  + You must know memory usage prior to execution
  + You have access to all source code
  + Coding standards likely preclude problematic constructs

--------------
Capabilities
--------------

+ Supports both Ada and C (and C++ "soon")
+ Supports all targets (native and cross)
+ Allows any optimization level
+ Handles concurrent programs
+ Handles dynamic dispatching
+ Available on command-line
+ Integrated with GNAT Programming Studio (GNAT Pro IDE)

---------------------------------
Why Analyze Stack Requirements?
---------------------------------

+ Preventing stack overflow is easier (less costly)

  + Detection is complex and incurs overhead
  + Debugging is difficult and expensive

    + Effect of undetected stack overflow is unpredictable
    + Code that overflows may not be where error appears
    + Not always corrupting the same data

+ May be required

  + Not all systems allow dynamic stack growth
  + Required by standards for high-integrity systems

    + DO-178B, etc.

+ Better to know it cannot happen in the first place

---------------------------------------
Run-time Detection Can Be Problematic
---------------------------------------

+ Difficult to achieve well in all cases

  + "Probing" imposes run-time costs you cannot recoup
  + "Guard pages" are not guaranteed to catch overrun
  + Issues can be addressed (GNAT does) but not w/o expense
  + 
  + 
  + 
  + 
  + 
  + 

+ Difficult to recover, if detected

  + What has been corrupted?
  + Is there sufficient stack space for recovery steps?
+ 
+ Allocated
+ Page
+ Guard
+ Page
+ Allocated
+ Page
+ Allocated
+ Page
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ Traps on access
+ Current Utilization
+ *Requested*
+ 
+ Application *N*

---------------------
Required by DO-178B
---------------------

+ Table A-5
+ Verification Of Outputs of Software Coding & Integration Process
+ 
+ 
+ 
+ 
+ 
+ Should be satisfied with independence.
+ Should be satisfied.

-------------------------------------
Accuracy & Consistency Requirements
-------------------------------------

+ **6.3.4 Reviews and Analyses of the Source Code**
+ 
+ [...]
+ 
+ f. Accuracy and consistency: The objective is to determine the correctness and consistency of the Source Code, including **stack usage** , fixed point arithmetic overflow and resolution, resource contention, worst-case execution timing, exception handling, use of uninitialized variables or constants, unused variables or constants, and data corruption due to task or interrupt conflicts.

------------------------
Dynamic Stack Analysis
------------------------

+ Requires running or simulating the application
+ Measures max amount of memory used for that execution
+ Supported by GNAT

  + Each task can report max usage when it terminates
  + See " Dynamic Stack Usage Analysis " in the GNAT Pro User Guide for details

+ How to know the worst case is measured?

-----------------
Dynamic Testing
-----------------

+ 
+ 40 bytes
+ 
+ Main
+ 
+ 125 bytes
+ 
+ Integral
+ 
+ 40 bytes
+ 
+ New_Value
+ 
+ 15 bytes
+ 
+ Open_Gas
+ 
+ 120 bytes
+ 
+ Activate
+ 
+ 45 bytes
+ 
+ Get_Value
+ 
+ 60 bytes
+ 
+ Compute
+ 
+ 
+ 
+ **Stack Usage:**
+ **145 bytes**
+ **Stack Usage:**
+ **265 bytes**
+ **Stack Usage:**
+ **175 bytes**

-----------------------------
The Missing Worst Case Path
-----------------------------

+ 
+ 40 bytes
+ 
+ Main
+ 
+ 125 bytes
+ 
+ Integral
+ 
+ 40 bytes
+ 
+ New_Value
+ 
+ 15 bytes
+ 
+ Open_Gas
+ 
+ 120 bytes
+ 
+ Activate
+ 
+ 45 bytes
+ 
+ Get_Value
+ 
+ 60 bytes
+ 
+ Compute
+ 
+ **Stack Usage:**
+ **145 bytes**
+ **Stack Usage:**
+ **265 bytes**
+ **Stack Usage:**
+ **175 bytes**
+ **Stack Usage:**
+ **270 bytes**

-----------------------
Static Stack Analysis
-----------------------

+ Does not involve executing the application
+ Can determine actual worst case utilization
+ Analyzes either source or object code

  + Per-subprogram stack consumption
  + Control-flow analysis

+ Supported by GNATstack
+ How to handle dynamic constructs known only at run-time?

  + Example: access-to-subprograms
  + Requires tool support
  + May require supplemental input from user

----------------------------------
Static Stack Analysis Techniques
----------------------------------

+ Object code analysis

  + Target dependent
  + Typically requires heuristics to cope with:

    + Different compilers or versions of same compiler
    + Different optimizations

  + Convenient: you always have access to the object code

+ Source code analysis

  + Target independent
  + No problem with optimization levels
  + Performed by compiler
  + Requires access to all the sources

    + May be problematic with COTS

-------------------
Why the Compiler?
-------------------

+ Knows reality

  + Stack allocations
  + Subprogram calls

+ Has access to semantic information

  + Tasks, subprogram scopes, type ranges, etc.
  + 
  + 
  + 
  + 

    + 
    + 

+ Is target-independent and optimization-aware
+ **type** Index **is range** 1 .. 5;
+ 
+ **procedure** P (Upper : Index) **is**
+ Data : **array** (1 .. Upper) **of** Character;
+ **begin**
+ ...
+ **end** P;
+ The compiler knows worst case size of Data
+ 

----------------------
Source Code Analysis
----------------------

+ Requires sources for all routines actually called
+ Provided by AdaCore, if any
+ 
+ Get from O.S.
+ vendor, if any
+ 
+ *Application*
+ *Source Code*
+ *Operating System*
+ *Source Code*
+ *Run-Time Library*
+ *Source Code*

--------------------
GNATstack Workflow
--------------------

+ 
+ Ada and C Sources
+ graph: { title: "C:\simple
+ node: { title: "_Unwind_
+ edge: {sourcename: "_ada_
+ edge: {sourcename: "_ada_
+ }
+ .ci
+ Files
+ XML
+ Report
+ 
+ 
+ package R is
+ ...
+ ...
+ ...
+ end R;
+ package Q is
+ ...
+ ...
+ ...
+ end Q;
+ package P is
+ ...
+ ...
+ ...
+ end P;
+ 
+ 
+ Optional
+ user inputs for stack sizes and call cycle bounds
+ ......................
+ .....................
+ .......................
+ ......................
+ .....................
+ .......................
+ 0001110111
+ 000110
+ 000111100101010
+ 0000111101
+ 0001110111
+ 000110
+ 000111100101010
+ 0000111101
+ 0001110111
+ 000110
+ 000111100101010
+ 0000111101
+ Object & ALI
+ Files
+ 
+ 
+ 
+ **GNAT Pro Compiler**
+ Stack usage and call graph info
+ 
+ 
+ **GNATstack**
+ VCG
+ Textual Report
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ graph: { title: "C:\simple
+ node: { title: "_Unwind_
+ edge: {sourcename: "_ada_
+ edge: {sourcename: "_ada_
+ }
+ graph: { title: "C:\simple
+ node: { title: "_Unwind_
+ edge: {sourcename: "_ada_
+ edge: {sourcename: "_ada_
+ }

-----------------------------------
GNATstack Command Line Invocation
-----------------------------------

+ gnatstack  {switches}  [<.ci files> | <.o files>]
+ One or more stack info files from compiler (and users)
+ 
+ 
+ One or more object files containing embedded stack info
+ Can include project file switch and name
+ -P *project-filename* .gpr
+ 
+ gnatstack -P demo.gpr
+ GPR file can specify everything required

.. image:: c:\temp\images\slide16_1.png

+ Recommended
+ Approach
+ gnatstack -Wa -v obj\*.ci

-------------------
Available Outputs
-------------------

+ Maximum stack requirement per subprogram
+ Maximum stack requirement per subprogram call chain
+ N subprograms with largest stack requirement

  + Where N is chosen by user

+ Subprogram call chain with largest stack requirement
+ Maximum stack requirement per task

---------------
Demonstration
---------------

+ Scenario

  + No run-time system
  + Bare-machine, i.e., no operating system

+ This is a realistic scenario, but not universal

  + Typical for extremely stringent high-reliability apps

+ Uses a GNAT project file for sake of realism

.. image:: c:\temp\images\slide18_1.jpeg


-------------------
Demo Project File
-------------------

+ 
+ **project** Demo **is**
+ **for** Main **use** ("demo.adb");
+ **for** Runtime("Ada") **use** "zfp";
+ **package** Compiler **is**
+ **for** Default_Switches ("ada") **use** ("-fcallgraph-info=su");
+ **end** Compiler;
+ **package** Stack **is**
+ **for** Switches **use** ("-Wa", "-v");
+ **end** Stack;
+ **package** Builder **is**
+ **for** Default_Switches ("ada") **use** ("-g");
+ **end** Builder;
+ **end** Demo;
+ Required
+ 
+ Be verbose
+ Warnings
+ 
+ 
+ 
+ GNATstack
+ (optional)
+ 
+ " su " for stack utilization
+ "da" for dynamic allocation
+ " su,da " for both

-------------------------------------
Application Main Procedure demo.adb
-------------------------------------

+ procedure **Demo** is
+ 
+ type **Data_Type** is array **(1 .. 5)** of **Integer;**
+ 
+ function **Inverse (Input : Data_Type)** return **Data_Type** is
+ **Result : Data_Type;**
+ begin
+ for **Index** in **Data_Type'Range** loop
+ **Result (Index) := Input (Data_Type'Last - (Index - Data_Type'First));**
+ end loop **;**
+ return **Result;**
+ end **Inverse;**
+ 
+ **Data   : Data_Type := (1, 2, 3, 4, 5);**
+ **Result : Data_Type;**
+ 
+ begin
+ **Result := Inverse (Data);**
+ end **Demo;**

----------------------------------
Demo Tools Invocations & Results
----------------------------------

+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ c:\demo> gprbuild -P demo.gpr -v
+ gcc ... -fcallgraph-info=su ... \demo.adb
+ ...
+ 
+ c:\demo> gnatstack -P demo.gpr
+ GNATstack Pro 18.0 (20171010)
+ Copyright 2005-2017, AdaCore
+ 
+ ...
+ Accumulated stack usage information for entry points
+ 
+ main : total 124 bytes
+ +-> main at main:b__demo.adb:20:4 : 32 bytes
+ +-> demo at ... \demo.adb:1:1,ada_main_program:b__demo.adb:17:14 : 48 bytes
+ +-> demo.inverse at ... \demo.adb:5:9 : 64 bytes
+ 
+ GNATstack: analysis successfully finished
+ Individual values for a total of 124 bytes
+ 
+ 
+ 
+ Invoke GNATstack
+ Invoke compiler

------------------------------
Why Three Demo Entry Points?
------------------------------

+ Binder-generated Ada entry point for executable

  + Elaborates library units
  + Initializes run-time library
  + Invokes application main procedure

+ Application main procedure
+ Routine called by the main procedure
+ 
+ 
+ 
+ ...
+ 
+ main : total 124 bytes
+ +-> main at main:C:\demo\obj\b~demo.adb:19:4 : 32 bytes
+ +-> demo at ada_main_program:C:\demo\obj\b~demo.adb:16:14,Demo:C:\demo\src\demo.adb:1:1 : 48 bytes
+ +-> demo.inverse at Inverse:C:\demo\src\demo.adb:5:4 : 64 bytes
+ 
+ GNATstack: analysis successfully finished
+ 1
+ 
+ 2
+ 
+ 3
+ 

---------------------------------------
Actual Program Entry-Point b~demo.adb
---------------------------------------

+ 
+ 
+ 
+ **package body** ada_main **is**
+ 
+  **procedure** adafinal **is** ...
+ 
+ **procedure** adainit **is** ...
+ 
+ **procedure** Ada_Main_Program;
+ **pragma** Import (Ada, Ada_Main_Program, "_ada_demo");
+ 
+ **procedure** main **is**
+ **** ...
+ 
+ **begin**
+ **** adainit;
+  **** Ada_Main_Program;
+ **end;**
+ 
+ **end** ada_main;

---------------------------------------
Actual Program Entry-Point b~demo.adb
---------------------------------------

+ **Lab**

-----------
Basic Lab
-----------

+ Open a command prompt shell
+ Go (cd) to the "basic_lab" directory under the "gnatstack_labs" directory
+ Build the program
+ 
+ Invoke stack analysis
+ 
+ Examine results
+ gprbuild -P lab.gpr
+ gnatstack -P lab.gpr

-------------------
Input Source File
-------------------

+ **procedure** Demo **is**
+ 
+ **procedure** C **is**
+ X : **array** (1 .. 5) **of** Float;
+ **begin**
+ X := ( **others** => 1.0);
+ **end** C;
+ 
+ **procedure** B **is**
+ X : **array** (1 .. 5) **of** Boolean;
+ **begin**
+ X := ( **others** => True);
+ C;
+ **end** B;
+ 
+ **procedure** A **is**
+ X : **array** (1 .. 10) **of** Integer;
+ **begin**
+ X := ( **others** => 0);
+ B;
+ **end** A;
+ 
+ **begin**
+ A;
+ **end** Demo;

-------------------
Basic Lab Results
-------------------

+ 
+ GNATstack Pro 1.7.1 (20140108)
+ Copyright 2005-2011, AdaCore
+ 
+ Worst case analysis is *not* accurate because of external calls.
+ 
+ List of reachable external subprograms:
+ 
+ <__gnat_install_handler> at install_handler:C:\labs\gnatstack_labs\basic_lab\obj\b~demo.adb:48:17
+ <__main>
+ system.secondary_stack'Elab_Body at e25b:C:\labs\gnatstack_labs\basic_lab\obj\b~demo.adb:81:29
+ system.soft_links'Elab_Body at e23b:C:\labs\gnatstack_labs\basic_lab\obj\b~demo.adb:79:24
+ 
+ Accumulated stack usage information for entry points
+ 
+ main : total 400 bytes
+ +-> main at main:C:\labs\gnatstack_labs\basic_lab\obj\b~demo.adb:88:4 : 80 bytes
+ +-> demo at ada_main_program:C:\labs\gnatstack_labs\basic_lab\obj\b~demo.adb:85:14,
+ Demo:C:\labs\gnatstack_labs\basic_lab\src\demo.adb:1:1 : 64 bytes
+ +-> demo.a at A:C:\labs\gnatstack_labs\basic_lab\src\demo.adb:16:4 : 112 bytes
+ +-> demo.b at B:C:\labs\gnatstack_labs\basic_lab\src\demo.adb:9:4 : 80 bytes
+ +-> demo.c at C:C:\labs\gnatstack_labs\basic_lab\src\demo.adb:3:4 : 64 bytes
+ 
+ GNATstack: analysis successfully finished

-------------------
Basic Lab Results
-------------------

+ **GPS Integration**

-------------------------------
GPS Integration: Primary Menu
-------------------------------


.. image:: c:\temp\images\slide29_1.png

+ Requires prior compilation
+ Only if gnatstack found (by GPS) on the path

-------------------------------------------
"Analyze Stack Usage" Intermediate Dialog
-------------------------------------------


.. image:: c:\temp\images\slide30_1.png

+ Editable field
+ 
+ Invocation to be issued
+ 

-------------------------------------------
"Analyze Stack Usage" Intermediate Dialog
-------------------------------------------


.. image:: c:\temp\images\slide31_1.png

+ Non-textual annotations
+ 
+ 
+ Largest call chain stack requirement

-------------------------------------------
"Analyze Stack Usage" Intermediate Dialog
-------------------------------------------


.. image:: c:\temp\images\slide32_1.png

+ Textual report output
+ Known values
+ 
+ 
+ 

-------------------------------------------
"Analyze Stack Usage" Intermediate Dialog
-------------------------------------------

+ **Lab**

---------------------
GPS Integration Lab
---------------------

+ Go to the "basic" directory underneath the " gnatstack_labs " directory
+ Open GPS on the project file there

  + Can just enter " gps " on the command line in that directory
  + Or double-click on the GPS icon and browse to the project

+ Build the program

  + Build -> Project -> demo.adb
  + Or use the toolbar icon

+ Invoke stack analysis

  + Analyze -> Stack Analysis -> Analyze Stack Usage

.. image:: c:\temp\images\slide34_1.png


-------------------------
GPS Integration Lab (2)
-------------------------

+ Click on the "Untitled" tab at the top for overview
+ This is an easy way to see if there is a problem

.. image:: c:\temp\images\slide35_1.png

+ 
+ 

-------------------------
GPS Integration Lab (3)
-------------------------

+ Scroll up and down within the "b~demo.adb" file

.. image:: c:\temp\images\slide36_1.png

+ Amount for this frame
+ 
+ Running total
+ 

-------------------------
GPS Integration Lab (4)
-------------------------

+ Click on the line in the Locations view containing the entry for "demo.a"

.. image:: c:\temp\images\slide37_1.png


-------------------------
GPS Integration Lab (5)
-------------------------


.. image:: c:\temp\images\slide38_1.png

+ Amount for this frame
+ 
+ Running total
+ 

-------------------------
GPS Integration Lab (5)
-------------------------

+ **Challenging Cases**

-------------------
Challenging Cases
-------------------

+ Cycles in the calling sequences

  + How many times should the stack frames be added?
  + Number of calls depends on execution-time information

+ Unbounded stack frames

  + Size depends on execution-time information

+ "External" calls

  + Corresponding sources are not available for analysis

+ "Indirect" calls

  + Calls to targets via pointers
  + Target subprogram depends on execution-time information

.. image:: c:\temp\images\slide40_1.png


.. image:: c:\temp\images\slide40_2.wdp


---------------------------------------
Useful Warnings for Challenging Cases
---------------------------------------

+ Enabled via - Wxx switch

  + where *xx* is one or more characters as follows

+ 
+ 
+ 
+ 
+ 
+ In practice, enable all of them
+ -Wei
+ -Wc
+ **c** cycles
+ **u** unbounded frames
+ **e** external calls
+ **i** indirect calls
+ **a** all optional warnings

.. image:: c:\temp\images\slide41_1.png

+ -Wa

------------------------------------
Challenge: Cycles In Control Graph
------------------------------------

+ Worst case depends on number of iterations
+ Solutions

  + Avoid cycles! (typical with high-integrity standards)
  + Do control- and data-flow analyses to see if bounded
  + Get user input for max iterations
+ 10 times
+ 
+ Counter : Integer := 0;
+ 
+ **procedure** A **is**
+ **begin**
+ Counter := Counter + 1;
+  **if** Counter <= 10 **then**
+ B;
+  **end if** ;
+ **end** A;
+ 
+ 
+ **procedure** B **is**
+ **begin**
+ A;
+ **end** B;

.. image:: c:\temp\images\slide42_1.png


.. image:: c:\temp\images\slide42_2.wdp

+ 
+ 

----------------------------------------
Result of Analysis When Cycles Present
----------------------------------------


.. image:: c:\temp\images\slide43_1.png


-----------------------------
Example for Handling Cycles
-----------------------------

+ incomplete

-----------------------------
Challenge: Unbounded Frames
-----------------------------

+ Stack size may depend upon run-time information

  + 
  + 
  + 
  + 
  + 
  + 

+ Solutions

  + Avoid such objects (typical for high-integrity standards)
  + Use reduced range types in source code
  + Get user to provide stack utilization info
+ 
+ 
+ **type** Index **is new** Integer;
+ 
+ **procedure** P (Upper : Index) **is**
+ Data : **array** (1 .. Upper) **of** Character;
+ **begin**
+ ...
+ **end** P;
+ 
+ **function** "&" (Left, Right : String) **return** String **is**
+ Result : String (1 .. Left'Length + Right'Length);
+ **begin**
+ ...
+  **return** Result;
+ **end** "&";

.. image:: c:\temp\images\slide45_1.png


.. image:: c:\temp\images\slide45_2.wdp


----------------------------------
Sample Unbounded Frames Scenario
----------------------------------

+ **procedure** Demo **is**
+ 
+ **type** Index **is new** Integer;
+ 
+ **procedure** Silly (Upper : Index) **is**
+ Data_List : **array** (1 .. Upper) **of** Integer;
+ **begin**
+ **for** Value **of** Data_List **loop**
+ Value := 0;
+ **end loop** ;
+ **end** Silly;
+ 
+ **begin**
+ Silly (Upper => 1024);
+ **end** Demo;
+ > powerpc-elf-gnatmake -P demo.gpr
+ powerpc-elf-gcc -c -g -fcallgraph-info=su,da -I- -gnatA C:\demo\src\demo.adb
+ ...

--------------------------------------
Unbounded Frames Indicated In Report
--------------------------------------

+ 
+ 
+ 
+ > powerpc-elf-gnat stack -P demo.gpr
+ ...
+ Worst case analysis is *not* accurate because of unbounded frames.
+ 
+ List of reachable subprograms with dynamic unbounded frames:
+ 
+ In demo.silly at Silly:C:\demo\src\demo.adb:5:4
+ Data_List at C:\demo\src\demo.adb:6:7
+ 
+ Accumulated stack usage information for entry points
+ 
+ main : total 128+? bytes (unbounded)
+ +-> main at main:C:\demo\obj\b~demo.adb:19:4 : 32 bytes
+ +-> demo at ada_main_program:C:\demo\obj\b~demo.adb:16:14,
+ Demo:C:\ demo \src\demo.adb:1:1 : 16 bytes
+ +-> demo.silly at Silly:C:\demo\src\demo.adb:5:4 : 80+? bytes (unbounded)
+ 
+ GNATstack: analysis successfully finished

----------------------------------
Steps To Handle Unbounded Frames
----------------------------------

+ 
+ Determine the stack size

  + Manually or some other way

+ Create a user-defined ".ci" file specifying that size
+ Specify the new file to the project file

  + File names are arbitrary, ".ci" extension suggested
  + Format:
+ 
+ node: { title: " *symbolic-unit-name* " label: " *size* bytes (static)" }
+ -- *optional comment ...*
+ 
+ Decimal number
+ 
+ Syntax will be explained shortly

-----------------------------------
Handling Unbounded Frames Example
-----------------------------------

+ From the call in the main program, we know the upper bound of the array is 1024

  + So Data_List requires 1024 * 4 bytes each => 4096 bytes

+ Calling a subprogram requires around 32 bytes

  + Just an approximation, but reasonable

+ Thus we say the stack requirement is 4128 bytes
+ We then create a user-defined .ci file for that input
+ 
+ node: { title: "demo__silly.1435" label: "4128 bytes (static)" }
+ -- 4096 + 32 for call itself
+ user_inputs.ci

---------------------------------------
Specifying User-Defined Input File(s)
---------------------------------------

+ 
+ **project** Demo **is**
+ 
+ **for** Main **use** ("demo.adb");
+ 
+ **package** Compiler **is**
+ **for** Default_Switches ("ada") **use** ("-fcallgraph-info=su");
+ **end** Compiler;
+ 
+ **package** Stack **is**
+ **for** Switches **use** ("-Wa", "-v",    ...    "user-inputs.ci");
+ **end** Stack;
+ 
+ **package** Builder **is**
+ **for** Default_Switches ("ada") **use** ("-g");
+ **end** Builder;
+ 
+ **end** Demo;
+ 
+ 
+ Or -files= *filename*
+ where *filename* is a text file containing names of .ci files
+ user_inputs.ci
+ node: { title: "demo__silly.1435" label: "4128 bytes (static)" }
+ -- 4096 + 32 for call itself

-------------------------------------
Unbounded Frames Resolved In Report
-------------------------------------

+ Stack analysis is now complete and accurate:
+ 
+ 
+ >powerpc-elf-gnat stack -P demo.gpr
+ GNATstack Pro 1.7.1 (20140108)
+ Copyright 2005-2011, AdaCore
+ 
+ Accumulated stack usage information for entry points
+ 
+ main : total 4176 bytes
+ +-> main at main:C:\demo\obj\b~demo.adb:19:4 : 32 bytes
+ +-> demo at ada_main_program:	C:\demo\obj\b~demo.adb:16:14,
+ Demo:C:\demo\src\demo.adb:1:1 : 16 bytes
+ +-> demo.silly at Silly:C:\demo\src\demo.adb:5:4 : 4128 bytes
+ 
+ GNATstack: analysis successfully finished

---------------------
Symbolic Unit Names
---------------------

+ 
+ For subprograms in packages

  + Format is *package-* *name__unit* *-name*
  + Lowercase

+ For overloaded subprograms

  + As above, plus number appended to all but first name
  + Numbers increment monotonically starting at 2

+ For nested subprograms

  + Format is outer-unit- name__inner -unit- name. *xxx*
  + *xxx* is a unique number appended to the symbol name
  + Compiler defines the number appended

    + So watch out for changes

  + You must determine the names chosen by the compiler
+ Two underscore characters
+ 

--------------------------------------
Sample Symbolic Names for Unit Names
--------------------------------------

+ **package** P **is**
+ **procedure** R;
+ **procedure** Q (This : Integer);
+ **procedure** Q (This : Float);
+ **end** P;
+ p__r
+ 
+ p__q
+ 
+ p__q__2
+ 
+ **procedure** Demo **is**
+ ...
+ **procedure** Silly (Upper : Index) **is**
+ ...
+ **begin**
+ **...**
+ **end** Silly;
+ 
+ **begin**
+ ...
+ **end** Demo;
+ demo__silly.1435
+ 

----------------------------
Getting the Symbolic Names
----------------------------

+ Search within the tool-generated .ci files
+ 
+ 
+ 
+ 
+ 
+ 
+ Search the object code
+ 
+ graph: { title: "C:\demo\src\demo.adb"
+ node: {	title: "_ada_demo"
+ label: "Demo\nC:\demo\src\demo.adb:1:1\n
+ 16 bytes (static)\n
+ 0 dynamic objects" }
+ edge: {	sourcename: "_ada_demo"
+ targetname: "C:\demo\src\demo.adb:demo__silly.1435"
+ label: "C:\demo\src\demo.adb:14:4" }
+ node: {	title: "C:\demo\src\demo.adb:demo__silly.1435"
+ label:	"Silly\nC:\demo\src\demo.adb:5:4\n
+ 80 bytes (dynamic)\n
+ 1 dynamic objects\n Data_List C:\demo\src\demo.adb:6:7" }
+ }
+ demo.ci
+ Declaration location
+ 

---------------------------------
Symbolic Names from Object Code
---------------------------------

+ 
+ C:\demo\obj>powerpc-elf-objdump --syms demo.o
+ 
+ demo.o:     file format elf32-powerpc
+ 
+ SYMBOL TABLE:
+ 00000000 l    df *ABS*  00000000 demo.adb
+ 00000000 l    d  .text  00000000 .text
+ 00000000 l    d  .data  00000000 .data
+ 00000000 l    d  .bss   00000000 .bss
+ 00000000 l     F .text  00000120 demo__silly.1435
+ 00000000 l    d  .debug_info    00000000 .debug_info
+ 00000000 l    d  .debug_abbrev  00000000 .debug_abbrev
+ 00000000 l    d  .debug_loc     00000000 .debug_loc
+ 00000000 l    d  .debug_aranges 00000000 .debug_aranges
+ 00000000 l    d  .debug_line    00000000 .debug_line
+ 00000000 l    d  .debug_str     00000000 .debug_str
+ 00000000 l    d  .debug_frame   00000000 .debug_frame
+ 00000000 l    d  .comment       00000000 .comment
+ 00000120 g     F .text  00000040 _ada_demo

---------------------------------
Symbolic Names from Object Code
---------------------------------

+ **Lab**

----------------------
Unbounded Frames Lab
----------------------

+ Open a command prompt
+ Go to the "unbounded_frames_lab" directory under the "gnatstack_labs" directory
+ Build the project
+ Invoke stack analysis

--------------------------
Unbounded Frames Lab (2)
--------------------------

+ Review the result

.. image:: c:\temp\images\slide58_1.png

+ 

--------------------------
Unbounded Frames Lab (3)
--------------------------


.. image:: c:\temp\images\slide59_1.png

+ GNATstack made a guess but indicates an issue
+ 

--------------------------
Unbounded Frames Lab (4)
--------------------------

+ Open the "Files" view (if not already open)

.. image:: c:\temp\images\slide60_1.png

+ 

--------------------------
Unbounded Frames Lab (5)
--------------------------

+ Open the "demo.ci" file in the obj directory

  + Double-click on it
  + 

.. image:: c:\temp\images\slide61_1.png

+ 

--------------------------
Unbounded Frames Lab (6)
--------------------------

+ Find the symbolic name for the Demo.Combine procedure within the "demo.ci" file

.. image:: c:\temp\images\slide62_1.png

+ 
+ "demo__combine.1591"
+ **Your number may be different!**

--------------------------
Unbounded Frames Lab (7)
--------------------------

+ How much does the call to Combine require?
+ **procedure** Demo **is**
+ 
+ A : String (1 .. 10) := ( **others** => 'A');  -- 10 chars
+ 
+ B : String (5 .. 14) := ( **others** => 'B');  -- 10 chars
+ ****
+  **procedure** Combine (Left, Right : String) **is**
+ Input : String (1 .. Left'Length + Right'Length);
+  **begin**
+ Input := ( **others** => ' ');
+ ...
+ **end** Combine;
+ 
+ **begin**
+ Combine (A, B);
+ **end** Demo;

--------------------------
Unbounded Frames Lab (8)
--------------------------

+ Use the Files view to open the file "user_inputs.ci"
+ Enter the user-defined size information in that file
+ 
+ 
+ Save the file
+ Re-invoke the stack analysis
+ node: { title: "demo__combine.1591" label: "52 bytes (static)" }
+ -- 20 + 32 for call itself

--------------------------
Unbounded Frames Lab (9)
--------------------------


.. image:: c:\temp\images\slide65_1.png

+ User-defined value used in gnatstack calculation

----------------------------------------
Challenge: External (Unresolved) Calls
----------------------------------------

+ Calls to routines for which corresponding source code is not available
+ Not unusual

  + COTS
  + Operating System libraries
  + et cetera

+ Solutions

  + Compile to get the information

    + Requires the sources

  + Get vendor input for required information
  + Get user to provide estimates

.. image:: c:\temp\images\slide66_1.png


.. image:: c:\temp\images\slide66_2.wdp


-----------------------------------
Sample Unresolved Calls In Output
-----------------------------------

+ Same demo main program but now a native compiler with full run-time and operating system
+ 
+ >gnatstack *.ci -Wa
+ Worst case analysis is *not* accurate because of external calls.
+ 
+ List of reachable external subprograms:
+ 
+ <_Unwind_Resume>
+ <__gnat_finalize>
+ <__gnat_initialize>
+ <__gnat_install_handler>
+ <__gnat_unhandled_except_handler>
+ <__main>
+ system.exception_table'Elab_Body
+ system.exceptions'Elab_Spec
+ system.secondary_stack'Elab_Body
+ system.soft_links'Elab_Body
+ system.soft_links'Elab_Spec
+ system.standard_library.adafinal
+ 
+ Accumulated stack usage information for entry points
+ 
+ main : total 224 bytes
+ +-> main
+ +-> demo
+ +-> demo.inverse
+ 
+ GNATstack: analysis successfully finished
+ 
+ Run-Time Library

-------------------------------
File "demo.ci" from GNATstack
-------------------------------

+ 
+ 
+ graph: { title: "C:\simple_project\demo.adb"
+ node: {	title: "_Unwind_Resume" label: "builtin_unwind_resume\n<built-in>" shape : ellipse }
+ node: {	title: "_ada_demo"
+ label: "Demo\n	C:\simple_project\demo.adb:1:1\n
+ 80 bytes (static)" }
+ edge: {	sourcename: "_ada_demo" targetname: "_Unwind_Resume" }
+ edge: {	sourcename: "_ada_demo"
+ targetname: "C:\simple_project\demo.adb:demo__inverse.2583"
+ label: "C:\simple_project\demo.adb:19:11" }
+ node: {	title: "C:\simple_project\demo.adb:demo__inverse.2583"
+ label: "Inverse\nC:\simple_project\demo.adb:5:4\n
+ 64 bytes (static)" }
+ }

------------------------------------
File "undefined.ci" from GNATstack
------------------------------------

+ 
+ 
+ node: {	title: "_Unwind_Resume" label: "XXX bytes" }
+ node: {	title: "__gnat_finalize"
+ label: "finalize\nC:\simple_project\b~demo.adb:122:17\nXXX bytes" }
+ node: {	title: "__gnat_initialize"
+ label: "initialize\nC:\simple_project\b~demo.adb:119:17\nXXX bytes" }
+ node: {	title: "__gnat_install_handler"
+ label: "install_handler\nC:\simple_project\b~demo.adb:63:17\nXXX bytes" }
+ node: {	title: "__gnat_unhandled_except_handler" label: "XXX bytes" }
+ node: {	title: "__main" label: "XXX bytes" }
+ node: {	title: "system__exception_table___elabb"
+ label: "e31b\nC:\simple_project\b~demo.adb:100:29\nXXX bytes" }
+ node: {	title: "system__exceptions___elabs"
+ label: "e33b\nC:\simple_project\b~demo.adb:102:24\nXXX bytes" }
+ node: {	title: "system__secondary_stack___elabb"
+ label: "e37b\nC:\simple_project\b~demo.adb:106:29\nXXX bytes" }
+ ...
+ "Stack Size Not Known"
+ 
+ 

----------------------------------
User-Supplied Stack Info File(s)
----------------------------------

+ 
+ **project** Demo **is**
+ 
+ **for** Main **use** ("demo.adb");
+ 
+ **package** Compiler **is**
+ **for** Default_Switches ("ada") **use** ("-fcallgraph-info=su");
+ **end** Compiler;
+ 
+ **package** Stack **is**
+ **for** Switches **use** ("-Wa", "-v",  ...  "user-inputs.ci");
+ **end** Stack;
+ 
+ **package** Builder **is**
+ **for** Default_Switches ("ada") **use** ("-g");
+ **end** Builder;
+ 
+ **end** Demo;
+ user_inputs.ci
+ 
+ node: { title: "_Unwind_Resume" label: "100 bytes" }
+ -- not the real number...
+ 

------------------------------------
Updated Unresolved Calls In Output
------------------------------------

+ 
+ 
+ >gnatstack -Wa *.ci
+ 
+ Worst case analysis is *not* accurate because of external calls.
+ 
+ List of reachable external subprograms:
+ 
+ <__gnat_finalize>
+ <__gnat_initialize>
+ <__gnat_install_handler>
+ <__gnat_unhandled_except_handler>
+ <__main>
+ system.exception_table'Elab_Body
+ system.exceptions'Elab_Spec
+ system.secondary_stack'Elab_Body
+ system.soft_links'Elab_Body
+ system.soft_links'Elab_Spec
+ system.standard_library.adafinal
+ 
+ Accumulated stack usage information for entry points
+ 
+ main : total 260 bytes
+ +-> main
+ +-> demo
+ +-> _Unwind_Resume
+ 
+ GNATstack: analysis successfully finished
+ No longer unresolved
+ 

------------------------------------------
GPS Support for User-Defined ".ci" Files
------------------------------------------

+ Opens file specified in project gpr file

  + Must already be created and specified by you, manually

.. image:: c:\temp\images\slide72_1.png


------------------------------------------
GPS Support for User-Defined ".ci" Files
------------------------------------------


.. image:: c:\temp\images\slide73_1.png

+ Entering a value here moves entry up to file
+ 
+ 

---------------------------
Challenge: Indirect Calls
---------------------------

+ Calls in which the destination routine is not known at compile-time

  + On purpose!
  + Pointers to subprograms

+ Solutions

  + Avoid such calls (typical with high-integrity standards)
  + Use semantic analysis by compiler

    + Find set of all candidate subprograms (not infinite!)
    + Not necessarily offered by compiler

  + Get user input from the set of candidate target routines

.. image:: c:\temp\images\slide74_1.png


.. image:: c:\temp\images\slide74_2.wdp


----------------------------
Indirect Call Example Spec
----------------------------

+ **package** Mouse **is**
+ **type** Buttons **is** (Left, Middle, Right);
+ **type** Response **is access procedure** ;
+ **procedure** Bind (B : **in** Buttons;
+ Command : **in** Response);
+ **procedure** Respond (Pressed : **in** Buttons);
+ **procedure** Beep;
+ **private**
+ Commands : **array** (Buttons) **of** Response :=
+ ( **others** => Beep'Access);
+ **end** Mouse;
+ **procedure** Beep **is**
+ **begin**
+ ...
+ **end** Beep;
+ Left
+ Middle
+ Right
+ 
+ 
+ 
+ 
+ 
+ 

----------------------------
Indirect Call Example Body
----------------------------

+ **with** GNAT.IO;
+ **package body** Mouse **is**
+ 
+ **procedure** Bind (B : **in** Buttons; Command : **in** Response) **is**
+ **begin**
+ Commands (B) := Command;
+ **end** Bind;
+ 
+ **procedure** Respond (Pressed : **in** Buttons) **is**
+ **begin**
+ Commands (Pressed). **all** ;
+ **end** Respond;
+ 
+ **procedure** Beep **is**
+ **begin**
+ GNAT.IO.Put (ASCII.Bel);
+ **end** Beep;
+ 
+ **end** Mouse;
+ Commands
+ Left
+ Middle
+ Right
+ 
+ 
+ 
+ **procedure** Bar **is**
+ **begin**
+ ...
+ **end** Bar;
+ **procedure** Foo **is**
+ **begin**
+ ...
+ **end** Foo;
+ 
+ 
+ 
+ Indirect call
+ 
+ **procedure** Baz  **is**
+ **begin**
+ ...
+ **end** Baz ;

------------------------------------
Indirect Call Example Main Program
------------------------------------

+ **with** Mouse; **use** Mouse;
+ **with** File_Ops; **use** File_Ops;
+ **procedure** Demo **is**
+ **begin**
+ Mouse.Respond (Pressed => Left);  -- beeps
+ Mouse.Bind (Button  => Left,
+ Command => Open_File'Access);
+ Mouse.Respond (Pressed => Left);
+ **end** Demo;
+ Commands
+ Left
+ Middle
+ Right
+ 
+ 
+ 
+ **procedure** Beep **is**
+ **begin**
+ ...
+ **end** Beep;
+ **procedure** Open_File  **is**
+ **begin**
+ ...
+ **end** Open_File ;
+ 
+ 
+ 

----------------------------------------
Indirect Call Example Initial Analysis
----------------------------------------

+ 
+ 
+ >powerpc-elf-gnat stack -P demo
+ 
+ Worst case analysis is *not* accurate because of indirect calls.
+ 
+ List of reachable and unresolved indirect (including dispatching) calls:
+ 
+ 1 indirect call in: mouse.respond
+ at C:\demo\src\mouse.adb:12
+ 
+ Accumulated stack usage information for entry points
+ 
+ main : total 80+? bytes
+ +-> main
+ +-> demo
+ +-> mouse.respond
+ +-> indirect call *
+ 
+ GNATstack: analysis successfully finished
+ Commands (Pressed). **all** ;
+ 

---------------------------------
Specify All the Target Routines
---------------------------------

+ 
+ 
+ The name of a routine
+ 
+ undefined.ciu
+ 
+ user-defined.ci
+ Location text must be identical (or not specified, and will globally match)
+ All possible targets for indirect call
+ edge: {	sourcename: "mouse__respond"
+ targetname: "XXX" label: "C:\demo\src\mouse.adb:12:26" }
+ edge: {	sourcename: "mouse__respond"
+ targetname: "file_ops__close_file" label: "C:\demo\src\mouse.adb:12:26" }
+ edge: {	sourcename: "mouse__respond"
+ targetname: "file_ops__open_file" label: "C:\demo\src\mouse.adb:12:26" }
+ 
+ 

----------------------------------------
Analysis with User-Defined Target Info
----------------------------------------

+ 
+ >powerpc-elf-gnat stack -P demo
+ Accumulated stack usage information for entry points
+ 
+ main : total 176 bytes
+ +-> main
+ +-> demo
+ +-> mouse.respond
+ +-> dereferencing to file_ops.close_file
+ 
+ GNATstack: analysis successfully finished
+ No remaining indirect calls
+ Largest target is used for analysis
+ 

-------------------------------------
Analysis with Switch "-ta" Included
-------------------------------------

+ 
+ 
+ >powerpc-elf-gnat stack -P demo -ta
+ Accumulated stack usage information for entry points
+ 
+ main : total 176 bytes
+ +-> main
+ +-> demo
+ +-> mouse.respond
+ +-> dereferencing to file_ops.close_file
+ 
+ List of dereference calls resolved by manual information
+ 
+ From subprogram mouse.respond:
+ Call at C:\demo\src\mouse.adb:12:26 may dereference:
+ +-> file_ops.close_file
+ +-> file_ops.open_file
+ 
+ GNATstack: analysis successfully finished
+ All user-specified targets are listed, for reference
+ 

---------------------------------
Support for Dynamic Dispatching
---------------------------------

+ Fully supported
+ Once you've built the executable, all possible dispatching targets are known
+ GNATstack will analyze all possible dispatching targets, per call, and use the ones with the largest stack requirements
+ Dynamic dispatching involves implicit indirect calls

  + By definition
  + Currently these must be manually handled

    + But will be automatically handled soon

-----------------------------
Dynamic Dispatching Example
-----------------------------

+ package **Graphics** is
+ type **Shape** is tagged
+ record
+ **X : Float := 0.0;**
+ **Y : Float := 0.0;**
+ end record **;**
+ function **Area (This : Shape)** return **Float;**
+ function **Momentum (This : Shape)** return **Float;**
+ end **Graphics;**
+ with **Graphics;**
+ package **Geometry** is
+ 
+ type **Circle** is new **Graphics.Shape** with
+ record
+ **Radius : Float;**
+ end record **;**
+ 
+ overriding
+ function **Area (This : Circle)** return **Float;**
+ 
+ end **Geometry;**
+ These really would be private types...
+ package body **Graphics** is
+ function **Area (This : Shape)** return **Float** is
+ begin
+ return **0.0;**
+ end **Area;**
+ function **Momentum (This : Shape)** return **Float** is
+ begin
+ return **This.X * Area (Shape'Class (This));**
+ end **Momentum;**
+ end **Graphics;**
+ with **Ada.Numerics;** use **Ada.Numerics;**
+ package body **Geometry** is
+ 
+ function **Area (This : Circle)** return **Float** is
+ begin
+ return **Pi * This.Radius**2;**
+ end **Area;**
+ 
+ end **Geometry;**
+ 

-----------------------------
Dynamic Dispatching Example
-----------------------------

+ 
+ with **Graphics;**
+ package **Repository** is
+ 
+ procedure **Register (This : Graphics.Any_Shape);**
+ 
+ procedure **Compute_All_Areas;**
+ 
+ end **Repository;**
+ with **GNAT.IO;**
+ package body **Repository** is
+ type **Node;**
+ type **Link** is access **Node;**
+ type **Node** is record
+ **Glyph : Graphics.Any_Shape;**
+ **Tail  : Link;**
+ end record **;**
+ **Head : Link;**
+ procedure **Register (This : Graphics.Any_Shape)** is
+ begin
+ **Head :=** new **Node'(Glyph => This, Tail => Head);**
+ end **Register;**
+ procedure **Compute_All_Areas** is
+ **Next   : Link;**
+ **Result : Float;**
+ begin
+ **Next := Head;**
+ while **Next /=** null loop
+ **Result := Next.Glyph.Area;  --** **dispatches**
+ **GNAT.IO.Put_Line (Result'Img);**
+ **Next := Next.Tail;**
+ end loop **;**
+ end **Compute_All_Areas;**
+ end **Repository;**
+ Glyph
+ Tail
+ 
+ 
+ **Glyph.all**
+ 
+ 
+ 
+ 
+ 
+ 
+ **Circle**
+ **Triangle**
+ **Rectangle**
+ **Head**
+ 
+ 
+ 
+ 
+ 
+ 

----------------------------------
Dynamic Dispatching Example Main
----------------------------------

+ with **Geometry;** use **Geometry;**
+ with **Graphics;** use **Graphics;**
+ with **Repository;**
+ procedure **Demo** is
+ **C : Any_Shape :=** new **Circle'(X => 1.0, Y => 2.0, Radius => 2.5);**
+ begin
+ **Repository.Register (C);**
+ **Repository.Compute_All_Areas;**
+ end **Demo;**

------------------------------
Output of GNATstack Analysis
------------------------------

+ 
+ C:\demo>i586-wrs-vxworks-gnat stack -P dev
+ Worst case analysis is *not* accurate because of external calls, indirect calls.
+ List of reachable external subprograms:
+ <__gnat_install_handler>
+ <__gnat_malloc>
+ <__gnat_rcheck_CE_Access_Check>
+ gnat.io.put_line
+ system.fat_llf'Elab_Spec
+ system.img_real.image_floating_point
+ system.secondary_stack'Elab_Body
+ system.soft_links'Elab_Body
+ 
+ List of reachable and unresolved indirect (including dispatching) calls:
+ 1 indirect call in: repository.compute_all_areas
+ at C:\demo\src\repository.adb:34
+ Accumulated stack usage information for entry points
+ demo : total 204 bytes
+ +-> demo
+ +-> demo
+ +-> repository.compute_all_areas
+ +-> dispatching to graphics.area
+ 
+ GNATstack: analysis successfully finished
+ Implementation of dynamic dispatching
+ 
+ Stack size of largest dispatching target is used
+ automatically
+ 

---------------------------------
Support for Concurrency (Tasks)
---------------------------------

+ Each task is a separate entry point because each has a distinct stack
+ Same basic issue will arise: unresolved tasking system calls into run-time library
+ Solution is the same: provide the numbers in user-defined .ci files for these run-time routines

  + We can help you do that

------------------------------------
Sample Concurrent Program Code (1)
------------------------------------

+ **package** Calculation **is**
+ 
+ **type** Data_Type **is array** (1 .. 5) **of** Integer;
+ 
+ **function** Inverse (Input : Data_Type) **return** Data_Type;
+ 
+ **end** Calculation;
+ **package body** Calculation **is**
+ 
+ **function** Inverse (Input : Data_Type) **return** Data_Type **is**
+ Result : Data_Type;
+ **begin**
+ **for** Index **in** Data_Type'Range **loop**
+ Result (Index) := Input (Data_Type'Last - (Index - Data_Type'First));
+ **end loop** ;
+ **return** Result;
+ **end** Inverse;
+ ****
+ **end** Calculation;

------------------------------------
Sample Concurrent Program Code (2)
------------------------------------

+ **package** Threads **is**
+ ****
+  **task** Inverter;
+ 
+ **end** Threads;
+ **with** Calculation; **use** Calculation;
+ **package body** Threads **is**
+ 
+ **task body** Inverter **is**
+ Data   : Data_Type := (1, 2, 3, 4, 5);
+ Result : Data_Type;
+ **begin**
+ Result := Inverse (Data);
+ **end** Inverter;
+ 
+ **end** Threads;
+ **with** Calculation; **use** Calculation;
+ **with** Threads;
+ **procedure** Demo **is**
+ **** Data   : Data_Type := (1, 2, 3, 4, 5);
+ Result : Data_Type;
+ **begin**
+ Result := Inverse (Data);
+ **end** Demo;
+ Entry points

----------------------------------------
Same Basic Results for Concurrent Apps
----------------------------------------

+ 
+ 
+ 
+ Worst case analysis is *not* accurate because of
+ - external calls
+ - indirect calls
+ 
+ List of reachable external subprograms:
+ <__gnat_install_handler>
+ <system__tasking__activation_chainIP>
+ ada.exceptions.reraise_occurrence_no_defer
+ ada.exceptions.save_occurrence
+ system.secondary_stack'Elab_Body
+ system.soft_links'Elab_Body
+ system.soft_links.get_gnat_exception
+ system.soft_links.get_jmpbuf_address_soft
+ system.soft_links.set_jmpbuf_address_soft
+ system.tasking.restricted.stages'Elab_Body
+ system.tasking.restricted.stages.activate_restricted_tasks
+ ...
+ 
+ List of reachable and unresolved indirect (including dispatching) calls:
+ 
+ 1 indirect call in threads.inverter
+ at C:\Users\rogers\Documents\AdaCore\training\GNATstack\tasking_demo\src\threads.adb:6
+ 
+ List of entry points:
+ demo
+ threads.inverter

-------------------------------------
GPS Integration: GNATstack Switches
-------------------------------------


.. image:: c:\temp\images\slide91_1.png


--------------------------
GNATstack Summary Points
--------------------------

+ Determines stack requirements prior to execution
+ Especially useful for high-integrity applications

  + You need to know memory usage
  + You have access to all source code
  + Coding standards preclude problematic cases
  + Might not be a run-time library or operating system

+ Integrated with GNAT Pro IDE (GPS)
+ Very general purpose

  + Ada and C
  + All targets
  + Any optimization level
  + Any entry point (including tasks)

+ Problematic cases can be dealt with
+ 

--------------------------
GNATstack Summary Points
--------------------------


.. image:: c:\temp\images\slide93_1.jpg


.. image:: c:\temp\images\slide93_2.png

