
***********
GNATcheck
***********

-----------------
GNATcheck Is...
-----------------
+ An automated coding standards checker
+ Capable of expressing a variety of rules

  + GNAT compiler warnings and style checks
  + Language-defined and GNAT-defined restrictions
  + Complexity metrics
  + Specific GNATcheck-defined rules (~80, and growing)

+ Qualified to DO-178 in several programs
+ A command-line tool
+ Integrated in GPS / GNATbench

   .. image:: c:\temp\images\slide2_1.png
      :height: 50%

   + Recommended approach

---------------------
Required by DO-178B
---------------------

   .. image:: c:\temp\images\do178_table_a5.jpg

--------------------------------------
Conformance To Standards Requirement
--------------------------------------

+ **6.3.4 Reviews and Analyses of the Source Code**
+ [...]
+ d. Conformance to standards: The objective is to **ensure that the Software Code Standards were followed** during the development of the code, especially complexity restrictions and code constraints that would be consistent with the system safety objectives. Complexity includes the degree of coupling between software components, the nesting levels for control structures, and the complexity of logical or numeric expressions. This analysis also ensures that deviations to the standards are justified.

---------------------------
GNATcheck Is An ASIS Tool
---------------------------
+ Source must be compilable

  + Warnings issued otherwise
  + GNATcheck will continue without analyzing them

+ All source dependencies must be available

  + Those units named in with-clauses, transitively
  + Whether or not they are to be analyzed themselves
+ **with** P;
+ **procedure** Demo **is** ...
+ **with** Terminal;
+ **package** P **is** ...
+ **package** Terminal **is** ...

-------------------------
Command Line Invocation
-------------------------
+ gnatcheck [options] {filename} {-files=filename} -rules rule_switches
+ Including project file switch and name
+ Specific rules, or name of a text file containing rules
+ -P project-filename.gpr
+ -from=filename
+ gnatcheck -P foo.gpr
+ gnatcheck  main.adb  -rules +RPositional_Parameters
+ Source file names;
+ wildcards allowed
+ Name of a file containing source file names
+ **or**

----------------------
Some Useful Switches
----------------------
+ "--help"
+ "-h"

  + List rule identifiers with very brief descriptions

+ "--show-rule"

  + Append rule names to messages

+ "- Xname =value"

  + Specify an external reference for argument in project file

+ "-o filename"

  + Specify the name of the report file
  + Default is " *toolprefix-* gnatcheck.out "

---------------------
Using Project Files
---------------------
+ The optimum approach
+ Convenient for multiple source directories
+ Convenient for checking multiple projects

  + Root project and dependencies

+ Usable with both command line and IDEs
+ File specified via switch "-P" as usual
+ Uses a tool-specific package named "Check"

------------------------------
Specifying Rules In GPR File
------------------------------
+ **project** Gnatcheck_Example **is**
+  **...**
+  **package** Check **is**
+ **for** Default_Switches ("Ada") **use**
+ ("-rules",
+ "+RAbstract_Type_Declarations",
+ "+RAnonymous_Arrays",
+ "+RLocal_Packages",
+ "+RFloat_Equality_Checks",
+ "+REXIT_Statements_With_No_Loop_Name",
+ "+RStyle_Checks:e");
+  **end** Check;
+ **end** Gnatcheck_Example;
+ Don't forget this

----------------------------
Rules File Use In GPR File
----------------------------
+ Convenient due to typically large number of rules
+ **project** Gnatcheck_Example **is**
+  **for** Source_Dirs **use** (...);
+  **package** Check **is**
+  **for** Default_Switches ("Ada") **use** ("-rules", "-from=coding_standard");
+  **end** Check;
+ **end** Gnatcheck_Example;
+ Arbitrary
+ File Name
+ Don't forget both parts

------------------------------------------
GNATcheck Switches In Project Properties
------------------------------------------

.. image:: c:\temp\images\slide11_1.png

+ As always, direct manual entry is also supported

-------------------
Basic Rule Syntax
-------------------
+ +R **<rule name>**

  + Activates rule specified

+ +R **<rule name : parameter>**

  + Activates rule specified, for the value of the parameter

+ -R **<rule name>**

  + Deactivates specified rule previously activated

+ -R **<rule name : parameter>**

  + Deactivates specified rule, depending on parameter value

+ -from **=** **rule_option_filename**

  + Textually includes rules from specified file name
  + Hence files can reference other files
+ Rule names are not case sensitive

-------------------
Sample Rules File
-------------------
+ -----------------------------------------------------
+ -- This is a sample gnatcheck coding standard file --
+ -----------------------------------------------------
+ --  First, turn on rules that are defined by gnatcheck
+ +RAbstract_Type_Declarations
+ +RAnonymous_Arrays
+ +RLocal_Packages
+ +RFloat_Equality_Checks
+ +REXIT_Statements_With_No_Loop_Name
+ --  Then, activate some checks defined by GNAT compiler:
+ +RStyle_Checks:e
+ --  This style check checks if a unit name is present
+ --  on END keyword that is the end of the unit declaration
+ Comments
+ Rules

-------------------------------
Sample Invocation Results (1)
-------------------------------
+ **package** Pack **is**
+ **type** T **is abstract tagged private** ;
+ **procedure** P (X : T) **is abstract** ;
+ **package** Inner **is**
+ **type** My_Float **is digits** 8;
+ **function** Is_Equal (L, R : My_Float) **return** Boolean;
+ **end** Inner;
+ **private**
+ **type** T **is abstract tagged null record** ;
+ **end** ;
+ **declaration of abstract type**
+ declaration of local package
+ declaration of abstract type
+ (style) "end Pack" required

-------------------------------
Sample Invocation Results (2)
-------------------------------
+ **package body** Pack **is**
+ **package body** Inner **is**
+ **function** Is_Equal (L, R : My_Float) **return** Boolean **is**
+ **begin**
+ **return** L = R;
+ **end** ;
+ **end** Inner;
+ **end** Pack;
+ use of equality operation for float values
+ (style) "end Is_Equal " required

------------------------------------------
Rule Exemptions Specified In Source Code
------------------------------------------
+ Uses GNAT-specific pragma Annotate

  + Used by source-oriented tools external to compiler
  + Syntax checked by compiler but no compilation effect

+ GNATcheck-specific usage


+ Usage errors are detected by GNATcheck
+ pragma **Annotate (** **identifier** **[,** **identifier** **{,** **arg** **}]);**
+ **pragma** Annotate ( gnatcheck , exemption_control, rule_name, [justification]);
+ exemption_control ::= Exempt_On | Exempt_Off
+ rule_name ::= string_literal
+ justification ::= string_literal

------------------------
Example Rule Exemption
------------------------
+ **procedure** Main **is**
+ **pragma** Annotate (gnatcheck, Exempt_On,  "Anonymous_Arrays",  "this one is **** fine");
+ Anon_Array : **array** (1 .. 10) **of** Float;
+ **pragma** Annotate (gnatcheck, Exempt_Off, "Anonymous_Arrays");
+ Another_Anon_Array : **array** (1 .. 10) **of** Integer;
+ ...
+ **begin**
+ ...
+ **end** Main;
+ Included in reports
+ anonymous array type
+ Exemption sections can be nested
+ Ignored

-----------------------------
Sample Report File Produced
-----------------------------
+ GNATCheck report
+ date              	: 2014-02-24 11:45
+ gnatcheck version	: gnatcheck Pro 7.3.0w (20140219-47)
+ command line      	: C:\GNATPRO\7.3.0w\bin\gnat.exe check -P gnatcheck_example.gpr
+ runtime           	: <default>
+ coding standard   	: coding_standard
+ list of sources   	: gnatcheck-source-list.out
+ 1. Summary
+ fully compliant sources               	: 0
+ sources with exempted violations only	: 0
+ sources with non-exempted violations	: 3
+ unverified sources                    	: 0
+ total sources                         	: 3
+ non-exempted violations               	: 8
+ rule exemption warnings               	: 0
+ compilation errors                    	: 0
+ exempted violations                   	: 1
+ 2. Exempted Coding Standard Violations
+ main.adb:6:23: anonymous array type  (this one is fine)
+ 3. Non-exempted Coding Standard Violations
+ main.adb:9:31: anonymous array type
+ main.adb:19:15: exit statement with no loop name
+ pack.adb:5:24: use of equality operation for float values
+ ...
+ " toolprefix-gnatcheck.out "

-----------------------------
Sample Report File Produced
-----------------------------
+ **Lab**

-------------------------------------------
Installation Verification & Basic Use Lab
-------------------------------------------
+ Open a command line prompt window
+ Go to the " gnatcheck /basic" folder in the folders provided
+ Invoke gnatcheck using the project file supplied
+ **gnatcheck** **-P** **gnatcheck_example.gpr** **-XPLATFORM=native**
+ Verify results
+ Leave the command prompt window open

----------------------------
Accessing the GNATcheck RM
----------------------------
+ From within GPS

  + In HTML
  + Via Help  :math:`\rightarrow` GNAT menu

+ Outside GPS, on the file system

  + Located under your GNAT Pro installation directory tree
  + Multiple file formats provided
  + In subdirs corresponding to the file format
  + File name is gnatcheck_rm .[pdf | html | txt | info]
  + Example:
+ C:\GNATPRO\7.2.1\share\doc\gnat\pdf\gnatcheck_rm.pdf
+ Default path

-----------------------------------
Accessing GNATcheck RM Within GPS
-----------------------------------

.. image:: c:\temp\images\slide22_1.png


----------------------------------
GNATcheck RM Sections In Browser
----------------------------------

.. image:: c:\temp\images\slide23_1.png

+ One big section, thus searchable

----------------------------------
GNATcheck RM Sections In Browser
----------------------------------
+ **Lab**

-----------------------------------
Accessing GNATcheck RM in GPS Lab
-----------------------------------
+ Use the command prompt window already open

  + At the "basic" lab directory

+ Open GPS by typing "gps" on the command line

  + GPS will find the one project file there and use it

+ Open the GNATcheck Reference Manual
+ Find the "Predefined Rules" chapter
+ Leave both GPS and the browser showing the GNATcheck Reference Manual open

------------------------------------------
GPS: Check All Sources In Single Project
------------------------------------------

.. image:: c:\temp\images\slide26_1.png

+ Right-click to display contextual menu
+ Click to invoke

------------------------------------------
GPS: Check All Sources In Single Project
------------------------------------------

.. image:: c:\temp\images\slide27_1.png

+ File for 1 st entry
+ Click to focus on source line

---------------------------------
GPS Source File Contextual Menu
---------------------------------

.. image:: c:\temp\images\slide28_1.png

+ Right-click to display contextual menu
+ Click to invoke

--------------------------------------
What Predefined Rules Are Available?
--------------------------------------
+ Defined by the language and AdaCore

  + Using pragma Restrictions

+ Defined by GNAT compiler

  + Style checks
  + Additional identifiers for pragma Restrictions

+ Defined by GNATcheck itself

  + Based on "Guide for the use of the Ada programming language in high integrity systems"  (ISO/IEC TR 15942)
  + Based on customers' certification requirements
  + Others...

+ All can be listed by GNATcheck with "-h" switch

  + Lists rule identifiers with very brief descriptions

-----------------------------------
"Predefined Rules" Categorization
-----------------------------------
+ Style-Related Rules

  + Object Orientation
  + Portability
  + Program Structure
  + Programming Practice
  + Readability
  + Tasking

+ Feature Usage Rules
+ Metrics-Related Rules
+ SPARK Ada Rules
+ Rules appear underneath the (sub)categories

---------------------------------
Rules for Compiler Style Checks
---------------------------------
+ Allows expressing compiler style checks as rules
+ Syntax


+ Example: enabling GNAT's built-in style checks

  + As compiler switch or pragma Warnings argument
  + As GNATcheck rule
+ +R Style_Checks : style-string-literals
+ +RStyle_Checks:y
+ -gnaty
+ +RStyle_Checks:yN
+ Enable
+ Disable

-----------------------------
Rules for Compiler Warnings
-----------------------------
+ Allows expressing compiler warning switches as rules
+ Syntax


+ Example: enabling (most of the) optional warnings

  + As compiler switch or pragma Warnings argument
  + As GNATcheck rule
+ +RWarnings: warning-string
+ +RWarnings:a
+ -gnatwa
+ +RWarnings:
+ Enable
+ Must use individual disabler chars
+ Note 's' is ignored

---------------------------------------
Rules for Language Restriction Checks
---------------------------------------
+ Allows expressing pragma Restrictions as rules

  + And GNAT-defined pragma Restriction_Warnings

+ Syntax


+ Example: disabling dynamic dispatching

  + As pragma Restrictions argument
  + As GNATcheck rule

+ Disabled using - RRestrictions with parameter
+ +RRestrictions : restrictions-parameter
+ +RRestrictions:No_Dispatch
+ **pragma** Restrictions (No_Dispatch);

-------------------------------------
Example for Detecting Implicit Code
-------------------------------------
+ **with** F; -- a function
+ **package** P **is**
+ Obj : **array** (1 .. F) **of** Integer;
+ **end** P;
+ An implicit heap allocation in GNAT
+ p.ads:3:04: warning: violation of restriction "No_Implicit_Heap_Allocations"
+ +RRestrictions: No_Implicit_Heap_Allocations -- defined by Ada
+ +RRestrictions:No_Implicit_Loops  			-- defined by GNAT
+ +RRestrictions:No_Implicit_Dynamic_Code 		-- defined by GNAT
+ +RRestrictions:No_Implicit_Conditionals 		-- defined by GNAT
+ Rules File

---------------------------------
Graphically Editing Rules Files
---------------------------------

.. image:: c:\temp\images\slide35_1.png

+ Same as via project contextual menu
+ Invokes dialog

--------------------------
Rules File Editor Dialog
--------------------------

.. image:: c:\temp\images\slide36_1.png

+ Editable; empty if no file specified already
+ Buttons invoking sub-dialogs
+ Editable; reflects choices via sub-dialogs

-----------------------------------
When Rules Files Contain Comments
-----------------------------------

.. image:: c:\temp\images\slide37_1.png

+ Even if you don't change anything, pressing Save removes the comments
+ verify and update screenshot
+ if needed

------------------------------------
The "Edit Rules File" Dialog Boxes
------------------------------------

.. image:: c:\temp\images\slide38_1.png


----------------------
Style Rules: Tasking
----------------------
+ Multiple_Entries_In_Protected_Definitions

  + Flags a protected definition with more than one entry
  + Processing for those with only one entry can be optimized

+ Volatile_Objects_Without_Address_Clauses

  + Flags each volatile object lacking an address clause
  + Two reasons for volatile objects:

    + Shared variable communication between tasks
    + Objects imported from external source set independently of Ada code
    + In this case an address clause will appear also

+ Weight_On_Wheels : Unsigned_8 **with**
+ **** Volatile,
+ Address => To_Address (16#DEAD_BEEF#);

----------------------
Style Rules: Tasking
----------------------
+ **Lab**

------------------------
Tasking Style Rule Lab
------------------------
+ Two reasons for volatile objects (at least):

  + Shared variable communication between tasks
  + Objects imported from external source and set externally

    + In this case an address clause will appear also

+ Some standards disallow shared variables for communicating between tasks
+ Use the rules file editor to specify the rule flagging volatile objects that do not have address clauses

  + Hint: Style
+ Weight_On_Wheels : Unsigned_8 **with**
+ **** Volatile,
+ Address => To_Address (16#DEAD_BEEF#);

------------------------
Tasking Style Rule Lab
------------------------

.. image:: c:\temp\images\slide42_1.png


---------------------------------
Style Rules: Object Orientation
---------------------------------
+ Deep_Inheritance_Hierarchies : **N**

  + Flags tagged or interface types with depth over given limit

+ Direct_Calls_To_Primitive **[:** **Except_Constructors** **]**

  + Flags non-dispatching calls to primitive operations
  + Optional parameter ignores "constructor" functions

+ Too_Many_Parents : **N**

  + Flags type declarations having more than *N* parents
  + Limits or prohibits multiple inheritance

+ Visible_Components

  + Flags public type declarations that declare visible components

+ Style_Checks:O

  + Flags overriding primitives that are not marked as such

-----------------
"Redispatching"
-----------------
+ An issue when one primitive operation calls another
+ Enables call to descendant overriding, if any

  + Without redispatching, a descendant is never called

+ Requires explicit conversion to a classwide type

  + Implicit in some languages and thus maintenance headache
+ **procedure** Primitive1 (X : **in** T) **is**
+ **begin**
+ ...
+ Primitive2 ( **T'Class** **(X)** );
+ ...
+ **end** Primitive1;
+ Redispatch based on the tag of the
+ actual parameter, not necessarily type T

----------------------------
Redispatching Illustration
----------------------------
+ **package** Graphics **is**
+  **type** Shape **is tagged**
+  **record**
+ X : Float := 0.0;
+ Y : Float := 0.0;
+  **end record** ;
+  **function** Area (This : Shape) **return** Float;
+  **function** Momentum (This : Shape) **return** Float;
+ **end** Graphics;
+ This would really be a private type

--------------------------------
Redispatching Illustration (2)
--------------------------------
+ **package body** Graphics **is**
+  **function** Area (This : Shape) **return** Float **is**
+  **begin**
+  **return** 0.0;
+  **end** Area;
+  **function** Momentum (This : Shape) **return** Float **is**
+  **begin**
+  **return** This.X * Area (This);
+  **end** Momentum;
+ **end** Graphics;

--------------------------------
Redispatching Illustration (3)
--------------------------------
+ **with** Graphics;
+ **package** Geometry **is**
+  **type** Circle **is new** Graphics.Shape **with**
+  **record**
+ Radius : Float;
+  **end record** ;
+  **overriding** **function** Area (This : Circle) **return** Float;
+ **end** Geometry;
+ **package body** Geometry **is**
+  **function** Area (This : Circle) **return** Float **is**
+  **begin**
+  **return** 3.14159265 * This.Radius**2;
+  **end** Area;
+ **end** Geometry;

--------------------------------
Redispatching Illustration (4)
--------------------------------
+ **with** Geometry; **use** Geometry;
+ **with** Ada.Text_IO; **use** Ada.Text_IO;
+ **procedure** Test_Geometry **is**
+ C : Geometry.Circle;
+ **begin**
+ C := (X => 1.0, Y => 2.0, Radius => 2.5);
+ Put_Line ("Area of C is " & Float'Image (Area (C)));
+ Put_Line ("Momentum of C is " & Float'Image ( Momentum (C) ));
+ **end** Test_Geometry;
+ **Result of call to Momentum will be** **zero!**

--------------------------------
Redispatching: Proper Approach
--------------------------------
+ **package body** Graphics **is**
+  **function** Area (This : Shape) **return** Float **is**
+  **begin**
+  **return** 0.0;
+  **end** Area;
+  **function** Momentum (This : Shape) **return** Float **is**
+  **begin**
+  **return** This.X * Are a ( Shape'Class (This) );
+  **end** Momentum;
+ **end** Graphics;
+ Redispatch to overriding from descendant types, if any

------------------------
Ensuring Redispatching
------------------------
+ graphics.adb:10:33: non-dispatching call to primitive operation
+ **package body** Graphics **is**
+  **function** Area (This : Shape) **return** Float **is**
+  **begin**
+  **return** 0.0;
+  **end** Area;
+  **function** Momentum (This : Shape) **return** Float **is**
+  **begin**
+  **return** This.X * Area (This);
+  **end** Momentum;
+ **end** Graphics;
+ +RDirect_Calls_To_Primitives: Except_Constructors
+ Optional Arg

--------------------------------------------
Enforcing Abstraction & Information Hiding
--------------------------------------------
+ **package** P **is**
+ **type** Content **is array** (Positive **range** <>) **of** Integer;
+ **type** Stack (Size : Positive) **is record**
+ Values : Content (1 .. Size);
+ Top    : Natural := 0;
+ **end record** ;
+ **procedure** Push (This : **in out** Stack;  Value : Integer) **with**
+ Pre => **not** Full (This);
+ **function** Full (This : Stack) **return** Boolean **is**
+ (This.Top = This.Size);
+ **end** P;
+ **Stack should be a private type!!**
+ Discriminants don't count


--------------------------------------------
Enforcing Abstraction & Information Hiding
--------------------------------------------
+ **Quiz**

--------------------
OO Style Rule Quiz
--------------------
+ Question: what is the rule for enforcing abstraction & information hiding for types -- i.e., private types?
+ Answer:
+ +RVisible_Components

+ p.ads:5:4: type defines publicly accessible components
+ **package** P **is**
+ **type** Content **is array** (Positive **range** <>) **of** Integer;
+ **type** Stack (Size : Positive) **is record**
+ Values	: Content (1 .. Size);
+ Top	: Natural := 0;
+ **end record** ;
+ **...**
+ **end** P;

---------------------------------
Controlling Dynamic Dispatching
---------------------------------
+ Restriction:No_Dispatch

  + Defined by language
  + No use of tagged types whatsoever

    + No polymorphism
    + But also no class-wide programming, even when it doesn't trigger dynamic dispatching

+ Restriction:No_Dispatching_Calls

  + Defined by AdaCore
  + Allows class-wide types and operations, but disallows any form of dynamic dispatching calls


--------------------------
Style Rules: Portability
--------------------------
+ Forbidden_Attributes and Forbidden_Pragmas

  + Can specify specific, named attributes/pragmas
  + Can indicate all GNAT-defined attributes/pragmas
  + Can indicate absolutely all attributes/pragmas

+ Predefined_Numeric_Type

  + Flags explicit use of a Standard numeric type or subtype
  + Overly pessimistic, e.g., flags String indexes

+ Separate_Numeric_Error_Handlers

  + Flags exception handlers for Constraint_Error without Numeric_Error, and vice versa
  + Recall Numeric_Error is a renaming of Constraint_Error now

+ Others...

--------------------------------
Style Rules: Program Structure
--------------------------------
+ Deeply_Nested_Generics

  + Flags nested generics with depth greater than N

+ Local_Packages

  + Flags all local packages inside package specs

+ Non_Visible_Exceptions

  + Flags constructs that may raise a local exception outside its scope

+ Raising_External_Exceptions

  + Flags raising exceptions that are neither predefined nor declared / renamed locally

-----------------------------------------
Exception Propagation Beyond Visibility
-----------------------------------------
+ **package** P **is**
+  **procedure** Q;
+ **end** P;
+ **package body** P **is**
+  Error : **exception** ;
+  **procedure** Q **is**
+  **begin**
+  **raise** Error;
+  **end** Q;
+ **end** P;
+ **with** P;
+ **procedure** Client **is**
+ **begin**
+ P.Q;
+ **exception**
+  **when** **P.Error** =>
+ ...
+ **end** Client;
+ Not visible!

-----------------------------------------
Exception Propagation Beyond Visibility
-----------------------------------------
+ **Quiz**

-----------------------------------
Program Structure Style Rule Quiz
-----------------------------------
+ Question: what is the rule for detecting exception propagation beyond the visibility of that name?
+ Answer:
+ +RRaising_External_Exceptions
+ **package body** P **is**
+  Error : **exception** ;
+  **procedure** Q **is**
+  **begin**
+  **raise** Error;
+  **end** Q;
+ **end** P;
+ p.adb:7:17: raised exception is not declared in
+ visible part of enclosing library package

-----------------------------------
Style Rules: Programming Practice
-----------------------------------
+ Anonymous_Arrays
+ Exceptions_As_Control_Flow
+ Exits_From_Conditional_Loops
+ EXIT_Statements_With_No_Loop_Name
+ GOTO_Statements
+ OTHERS_In_Exception_Handlers
+ Recursive_Subprograms
+ *Many* others...

-----------------
Beware "Others"
-----------------
+ A maintenance issue: compiler can't detect lack of specific handling when new values added
+ Case statement example

  + Suppose a new value **must** ** have a new case statement alternative, per application requirements
  + If you forget to add the new alternative, the compiler can't detect that fact because "others" covers new value too
+ **type** Space_Agencies **is**
+ **** (NASA, ESA, RFSA);
+ Bureau : Space_Agencies;
+ ...
+ **case** Bureau **is**
+ **when** ESA => ...
+ **when** NASA => ...
+ **when** others => ...
+ **end case** ;
+ ...
+ There are dozens of others!

-----------------
Beware "Others"
-----------------
+ **Quiz**

---------------------------------
Programming Practice Style Quiz
---------------------------------
+ Question: what are the rules for detecting the most important maintainability issues for "case statements"?
+ Answer:
+ OTHERS_In_CASE_Statements
+ Enumeration_Ranges_In_CASE_Statements

---------------------------
"Use Package" Clause Quiz
---------------------------
+ There has been much controversy over whether or not "use clauses" should be applied

  + Some say they decrease readability by removing info
  + Some say they help readability by reducing noise
  + Nowadays a decent IDE can tell you everything...

+ Question: what is the rule for detecting "use clauses" for packages?

  + Use-type clauses are ignored

+ Answer:
+ USE_PACKAGE_Clauses

.. image:: c:\temp\images\slide64_1.jpeg


--------------------------
Style Rules: Readability
--------------------------
+ Identifier_Casing

  + Flags defining identifiers whose letter casing does not respect specified scheme for their kind (type, constant, etc.)

+ Identifier_Prefixes

  + Flags defining identifiers whose prefix does not respect specified scheme for their kind

+ Identifier_Suffixes

  + Flags defining identifiers whose suffix does not respect specified scheme for their kind

+ Misnamed_Controlling_Parameters

  + Flags primitive operations whose controlling operand is both not the first and named "This"

+ Name_Clashes

  + Flags defining identifiers listed in a dictionary of disallowed names

+ Uncommented_BEGIN_In_Package_Bodies

  + Flags package body with an executable part that does not begin with a comment repeating the package name

---------------------
Feature Usage Rules
---------------------
+ Abstract_Type_Declarations
+ Anonymous_Subtypes
+ Blocks
+ Complex_Inlined_Subprograms
+ Controlled_Type_Declarations
+ Declarations_In_Blocks
+ Deeply_Nested_Inlining
+ Default_Parameters
+ Discriminated_Records
+ Explicit_Full_Discrete_Ranges
+ Float_Equality_Checks
+ Function_Style_Procedures
+ Generics_In_Subprograms
+ Implicit_IN_Mode_Parameters
+ Improperly_Located_Instantiations
+ Library_Level_Subprograms
+ Non_Qualified_Aggregates
+ Numeric_Literals
+ Parameters_Out_Of_Order
+ Raising_Predefined_Exceptions
+ Unassigned_OUT_Parameters
+ Unconstrained_Array_Returns

---------------------------------------------
Feature Usage Rule Example: "Magic Numbers"
---------------------------------------------
+ Numeric literals used instead of named numbers, constants, or attributes
+ Prevented using " Numeric_Literals " rule
+ Syntax

  + N : allow integer literals not exceeding this value
  + All : all integer literals are flagged
  + Statements_Only : numeric literals are flagged only when used in statements

+ If no parameters are set, max unflagged value is 1 and is not limited to statements
+ +RNumeric_Literals [:  N  |  All  |  Statements_Only]

----------------------------------------
Unassigned Mode OUT Parameters Example
----------------------------------------
+ Always a coding error
+ For scalar types, worse: "by-copy" mechanism will copy something back

  + May copy junk, with unpredictable effects
+ **procedure** Q (Input : **in** Integer;  Output : **out** Integer) **is**
+ **begin**
+ **if ... then**
+ **return** ;
+ **end if** ;
+ **end** Q;

----------------------------------------
Unassigned Mode OUT Parameters Example
----------------------------------------
+ **Quiz**

-------------------------
Feature Usage Rule Quiz
-------------------------
+ Question: what is the rule for detecting formal parameters of mode "out" that are not assigned within a subprogram?
+ Answer:
+ Note that any assignment to a mode out formal satisfies the check for that formal
+ Unassigned_OUT_Parameters
+ **procedure** Q
+ (Input  : **in** Integer;
+ Output : **out** Integer)
+ **is**
+ **begin**
+ **if** ... **then**
+ **return** ;
+ **end if** ;
+ Output := ...
+ **end** Q;
+ **This error is not detected by GNATcheck, maybe will be by the compiler, definitely will be by CodePeer**

-----------------------
Metrics-Related Rules
-----------------------
+ Metrics_Essential_Complexity : *N*

  + Flags a construct exceeding a value of *N* for essential complexity

+ Metrics_Cyclomatic_Complexity : *N*

  + Flags a construct exceeding a value of *N* for cyclomatic complexity

+ Metrics_LSLOC : *N*

  + Flags a compilation unit exceeding a value of *N* for LSLOC

--------------------------------------
SPARK Language Enforcement Rules (1)
--------------------------------------
+ Annotated_Comments

  + Flags comments used as SPARK annotations

+ Boolean_Relational_Operators

  + Flags calls to predefined relational operators for type Boolean

+ Expanded_Loop_Exit_Names

  + Flags expanded loop names in exit statements

+ Non_SPARK_Attributes

  + Flags attributes that are not defined in SPARK

+ Non_Tagged_Derived_Types

  + Flags derived types without a record extension part

--------------------------------------
SPARK Language Enforcement Rules (2)
--------------------------------------
+ Outer_Loop_Exits

  + Flags an exit statement that names an outer loop

+ Overloaded_Operators

  + Flags any declaration that overloads an operator symbol

+ Slices

  + Flags use of array slices

+ Universal_Ranges

  + Flags discrete ranges on universal_integer values, when used instead of named subtypes

----------------------
Combining Rule Forms
----------------------
+ You may need to apply a combination of the forms in order to achieve a specific effect

  + Compiler-defined rules (switches)
  + Ada pragma Restrictions
  + GNATcheck-defined rules

+ Any combination of the three forms is allowed

  + No need to use any one form

+ Their individual effects will overlap somewhat

---------------------------------------
Sample Combination: Boolean Operators
---------------------------------------
+ You may want to require use of short-circuit form

  + *and then* instead of *and* , *or else* instead of *or*
  + For sake of reducing complexity of MC/DC checks, etc.

+ Compiler style check rule + RStyle_Checks:B

  + Flags *and* as well as *or* for boolean types, except for simple variables and constants

+ Restriction rule + RRestrictions:No_Direct_Boolean_Operators

  + Flags operators *and* , *or* , and *xor* for boolean types

+ GNATcheck rule + RNon_Short_Circuit_Operators

  + Flags all calls to predefined *and*  and  *or* for booleans
  + Not flagged for use on modular types or boolean array types

-------------------------------------------
Sample Combination : Preventing Recursion
-------------------------------------------
+ Compiler always applies the style check rule

  + Not optional
  + Flags possible infinitely recursive calls
  + Can be justified with pragma Warnings(Off)

+ Restriction rule +RRestrictions:No_Recursion

  + Flags locally detected recursion
  + Program is erroneous if it uses recursion

+ GNATcheck rule +RRecursive_Subprograms

  + Flags all recursive chains of direct calls
  + Indirect calls via pointers are not detected

-----------------
Getting Started
-----------------
+ A switch is defined to facilitate rules file creation

  + Creates a file with name as specified

+ File contains all the rules, all turned off

  + A project file, if given, has no effect on content

+ Edit this file to get your own rules file
+ You will not use all the defined rules!

  + Many of them conflict with others
  + Define the subset that matches your (existing) code
+ gnatcheck --write-rules= file_name

-------------------
GNATcheck Summary
-------------------
+ An automated coding standards verifier/checker
+ Capable of expressing a variety of rules

  + GNAT compiler warnings and style checks
  + Language restrictions (via pragma Restrictions)
  + Complexity metrics (GNATmetric results)
  + Others, including SPARK related rules

+ You should not use all the existing rules!

  + Some of them conflict with others

+ Use switch "--help" for useful switches list

  + E.g., -j *n* for concurrent processing

-------------------
GNATcheck Summary
-------------------

.. image:: c:\temp\images\slide79_1.jpg


.. image:: c:\temp\images\slide79_2.png

