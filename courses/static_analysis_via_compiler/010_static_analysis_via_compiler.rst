*****************************
Static Analysis Via Compiler
*****************************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==========
Overview
==========

---------------------------
Typographical Styles Used
---------------------------

* :dfn:`This` is a definition
* :filename:`this/is/a.path`
* :ada:`code is highlighted`
* :command:`commands are emphasised --like-this`

--------------
Introduction
--------------

+ GNAT can be configured to perform static analysis

  + Warnings enabled via compiler switches

+ GNAT can be told that a subset of the language will be adhered to by the source code

  + Via language-defined :ada:`pragma Restrictions`
  + Affects code generation and run-time library candidates
  + Useful for certification

+ GNAT's analysis is extensive, but not without limitations

  + A compiler rather than a static analyzer
  + :toolname:`GNAT Static Analysis Suite` will be used as a counter-example

===============
GNAT Warnings
===============

--------------------
Warning Categories
--------------------

+ Definite errors
+ Probable errors
+ Possible mismatches with user expectations
+ Redundant code
+ Representation-related warnings

  + Biased integer representation, etc.

+ See *GNAT User's Guide* for all switches and their meanings

------------------------------------
Controlling Warnings With Switches
------------------------------------

+ Activated with option :command:`-gnatw[x]`

  + Where *x* is a character(s) specific to a warning

+ Deactivated with capitalized version of switch

  + E.g., :command:`-gnatwc` activates, :command:`-gnatwC` deactivates

+ :toolname:`GCC` back-end offers distinct warnings too
+ Warnings for nasty cases are enabled by default

  + Unintentional address clause overlays
  + Others...

------------------
Warnings Example
------------------

.. code:: Ada
   :number-lines: 1

   function Bad (B1, B2 : Boolean) return Integer is
      Result : Integer;
   begin
      Result := Result + 1;
      if B1 then
         return Result;
      end if;
      Result := Bad (B1, B2);
   end Bad;

:command:`gcc -c -gnatwa bad.adb`

.. container:: latex_environment tiny

  ::

    bad.adb:4:14: warning: "Result" may be referenced before it has a value [enabled by default]
    bad.adb:8:04: warning: possibly useless assignment to "Result", value might not be referenced [-gnatwm]
    bad.adb:8:11: warning: "return" statement missing following this statement [enabled by default]
    bad.adb:8:11: warning: Program_Error will be raised at run time [enabled by default]

-----------------
Definite Errors
-----------------

+ Compiler detects a runtime failure

  + Compiler can tell that an assertion is always false
  + Exceptions raised but not caught locally and :ada:`No_Exception_Propagation` restriction is applied

-------------------------
Definite Error Examples
-------------------------

.. code:: Ada
  :number-lines: 1

  pragma Restrictions (No_Exception_Propagation);
  procedure Test (Failure : Boolean) is
  begin
     if Failure then
      raise Constraint_Error;
   end if;
  end Test;

.. container:: latex_environment tiny

  ::

    test.adb:5:07: warning: pragma Restrictions (No_Exception_Propagation) in effect [-gnatw.x]
    test.adb:5:07: warning: execution may raise unhandled exception [-gnatw.x]

.. code:: Ada
  :number-lines: 1

  procedure Test (Param : in out Integer) is
  begin
     pragma Assert (Integer'object_size = 64);
     Param := Param + 1;
  end Test;

.. container:: latex_environment tiny

  ::

    test.adb:3:19: warning: assertion would fail at run time [-gnatw.a]

-----------------
Probable Errors
-----------------

+ Errors where compiler thinks coder made a mistake

  + Conditions that are always false or always true
  + Unused formal parameters

    + Can apply :ada:`pragma Unreferenced`, especially in OOP case

  + Variables that could be declared as constants

    + Not so much an error but should be heeded

  + Variables assigned but not read
  + Variables read but not assigned
  + Unchecked conversions with different source and target type sizes
  + Unlikely modulus value in type declaration
  + Suspicious actual parameter ordering
  + Missing parentheses may be confusing

-------------------------------
Probable Errors - Source Code
-------------------------------

.. code:: Ada
  :number-lines: 1

  with Unchecked_Conversion;
  package body Examples is

     function Convert is new Unchecked_Conversion (Integer, Character);
     type Mod_T is mod 2 * 32;

     procedure Example (A, B, C :     Natural;
                        D       : out Natural) is
        E : Natural := A * B;
        F : Natural;
     begin
        if E >= 0 then
           D := D + A / B;
           F := E;
        end if;
     end Example;

     procedure Test (A, B, C :     Integer;
                     D       : out Integer) is
     begin
        Example (A, C, B, D);
        D := -D mod B;
     end Test;

  end Examples;

---------------------------
Probable Errors - Results
---------------------------

.. container:: latex_environment tiny

  ::

    examples.adb:3:04: warning: types for unchecked conversion have different sizes [-gnatwz]
    examples.adb:4:24: warning: suspicious "mod" value, was ** intended? [-gnatw.m]
    examples.adb:6:13: warning: formal parameter "C" is not referenced [-gnatwu]
    examples.adb:8:07: warning: "E" is not modified, could be declared constant [-gnatwk]
    examples.adb:9:07: warning: variable "F" is assigned but never read [-gnatwm]
    examples.adb:11:12: warning: condition can only be False if invalid values present [-gnatwc]
    examples.adb:11:12: warning: condition is always True [-gnatwc]
    examples.adb:13:15: warning: "D" may be referenced before it has a value [enabled by default]
    examples.adb:21:07: warning: actuals for this call may be in wrong order [-gnatw.p]
    examples.adb:22:12: warning: unary minus expression should be parenthesized here [enabled by default]

--------------------------------
Probable Errors - Explanations
--------------------------------

* Line 5 - Coder probably meant :ada:`2 ** 32`

   * But maybe not? It could be a bit location

* Line 12 - :ada:`E` is :ada:`natural`, so it can never be less than zero (without invalid data)

* Line 13 - :ada:`D` is an :ada:`out` parameter, so there is no guarantee on it's initial value

* Line 22 - Did you mean :ada:`-(D mod B)` or :ada:`(-D) mod B`?

----------------
Redundant Code
----------------

+ Comparing boolean expression to boolean value
+ Type conversion when the entity is already of the target type

.. container:: speakernote

   + Use of attribute Base where :ada:`T'Base` is same as :ada:`T`
   I can't generate a warning for this

---------------------------
Redundant Code - Examples
---------------------------

.. code:: Ada
  :number-lines: 1

  package body Redundant_Code is

     procedure Test
       (A, B, C :        Integer;
        D       : in out Integer) is
     begin
        if (A > B) = True then
           D := D - 1;
        end if;
        D := D - Integer (C);
     end Test;

  end Redundant_Code;

.. container:: latex_environment tiny

  ::

    redundant_code.adb:7:18: warning: comparison with True is redundant [-gnatwr]
    redundant_code.adb:10:16: warning: redundant conversion, "C" is of type "Integer" [-gnatwr]

-------------------------------------------
Controlling Warnings With A Single Switch
-------------------------------------------

+ Switch :command:`-gnatwa` enables almost all warnings

  + Those typically useful
  + Good balance between actual problems and false positives

+ Switch :command:`-gnatw.e` enables absolutely all warnings

  + Including those not activated by :command:`-gnatwa`
  + Not recommended for typical use
  + Likely generates many warnings you'll end up ignoring
  + But you might want some of them, individually

----------------------------------------------
Highly Optional Warnings :command:`-gnatw.e`
----------------------------------------------

+ Implicit dereferencing (missing optional :ada:`.all`)
+ Activate tagging (warning messages tagged with certain strings)
+ Suspicious Subp'Access
+ Warnings for GNAT sources
+ Hiding (Potentially confusing hiding of declarations)
+ Holes/gaps in records
+ Redefinition of names in package :ada:`Standard`
+ Elaboration pragmas
+ List inherited aspects
+ Atomic synchronization
+ Modified but unreferenced parameters
+ Out of order record representation clauses
+ Overridden size clauses
+ Tracking of deleted conditional code
+ Unordered enumeration types
+ Warnings Off pragmss (flags unnecessary pragmas)
+ Activate information messages for why package needs a body

-----------------------------------------
Unordered Enumeration Value Comparisons
-----------------------------------------

+ Most enumerations are not semantically ordered

   .. code:: Ada

      -- not semantically ordered
      type Colors_T is (Red, Yellow, Green);
      -- semantically ordered
      type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

+ Comparisons other than equality are suspect

   .. code:: Ada
      :number-lines: 14

      if Current_Color > Yellow then -- must be Green, so go

+ Maintainers (you!) may change order later

   .. code:: Ada

      type Colors_T is (Green, Yellow, Red);

+ GNAT :ada:`pragma Ordered` can be used say that such comparisons make sense

   .. code:: Ada

      pragma Ordered (Days);

+ Can set warning :command:`-gnatw.u` to flag unordered relations

.. container:: latex_environment tiny

  ::

    examples.adb:14:32: warning: comparison on unordered enumeration type "Colors_t" declared at colors.ads:4 [-gnatw.u]

-------------------------------------------
Notifications of Deleted Conditional Code
-------------------------------------------

+ Also known as deactivated code
+ Applies to if-statements and case-statements
+ May be useful in certified applications

.. code:: Ada
   :number-lines: 3

   procedure Test (A : in out Integer) is
   begin
      if False then
         Put_Line ("Commented out for now");
      else
         Put_Line (A'Image);
      end if;
   end Test;

.. container:: latex_environment tiny

  ::

    examples.adb:6:10: warning: this code can never be executed and has been deleted [-gnatwt]

---------------------------------------------
Controlling Warnings Within the Source Text
---------------------------------------------

+ Via :ada:`pragma Warnings`

  + See **Implementation Defined Pragmas** in *GNAT Reference Manual*

+ Syntax

   + All have an optional string literal parameter :ada:`Reason` ignored by compiler but perhaps processed by other tools

``pragma Warnings ([TOOL_NAME,] DETAILS [, REASON]);``

``DETAILS ::= On | Off``

  * Enable/Disable all warnings

``DETAILS ::= On | Off, Local_Name``

  * Enable/Disable all warnings for :ada:`Local_Name`

``DETAILS ::= Static_String_Expression``

  * Enable/Disable warnings based on compiler switches specified in ``Static_String_Expression``

``DETAILS ::= On | Off, Static_String_Expression``

  * Enable/Disable all warnings based on warning message specified in ``Static_String_Expression``

``TOOL_NAME ::= SPARK | GNATprove``

  * Control which tool responds to pragma

``REASON ::= Reason => STRING_LITERAL {& STRING_LITERAL}``

  * Informational message that can be parsed by external tools

--------------------------------
Pragma Warnings Usage Examples
--------------------------------

+ All warnings off in this region of code only

   .. code:: Ada

      pragma Warnings (Off);
      Free (X);
      pragma Warnings (On);

+ All warnings off for this object, throughout its scope

   .. code:: Ada

      New_Tgt_Node : Counter;
      pragma Warnings (Off, New_Tgt_Node);

+ All warnings off that emit messages matching this text, in this region of code only

   .. code:: Ada

      -- Optional; matches any message text
      pragma Warnings (Off, "loop range is null*");
      --  On monoprocessor targets, the following loop will
      --  never execute (no other CPUs).
      for CPU_Id in CPU'First + 1 .. CPU'Last loop
         Start_CPU (CPU_Id);
      end loop ;
      pragma Warnings (On, "loop range is null*");


=====================
GNAT Style Checking
=====================

------------------
"Style" Checking
------------------

+ Style rules we use within AdaCore

  + Not a general coding standards checker (see :toolname:`GNATcheck`)
  + Some are arbitrary
  + Main thing is to be consistent

+ Categories of checks

  + Layout/presentation
  + Sound Engineering

+ Note that you don't have to use any/all of these!

---------------------------------
GNAT Style Enforcement Switches
---------------------------------

+ Activated with option :command:`-gnatyxx`

  + Where **xx** is replaced with list of style check parameters

+ Deactivated after minus (-):

  + :command:`-gnatyc` activates, :command:`-gnaty-c` deactivates

+ :command:`-gnaty` activates most style warnings (also :command:`-gnatyY`)

  + Equivalent to :command:`-gnaty3abcefhiklmnprst`
  + (Descriptions on following pages)

+ :command:`-gnatyN` suppresses all style warnings
+ See *GNAT User's Guide* section 3.2.5 for all the options available

------------
GNAT Modes
------------

+ Internal GNAT implementation mode :command:`-gnatg` |rightarrow| :command:`-gnatyg -gnatw.ge`
+ GNAT-Style mode :command:`-gnatyg` |rightarrow| :command:`-gnatyydISuxz`

  + ``y`` All standard check options
  + ``d`` No DOS line-terminators
  + ``I`` No **explicit** :ada:`in` keyword
  + ``S`` :ada:`then` / :ada:`else` statements on **different** line
  + ``u`` No unnecessary blank lines
  + ``x`` No extra parentheses in conditionals
  + ``z`` No extra parentheses in operations

+ GNAT source warnings :command:`-gnatw.g` (next slide)
+ Activate every optional warning :command:`-gnatw.e`

------------------------------------------
GNAT Source Warnings :command:`-gnatw.g`
------------------------------------------

+ *GNAT Source warnings* meaning may evolve and switches may change
+ As of now, :command:`-gnatw.g` |rightarrow| :command:`-gnatwAao.q.s.CI.V.X.Z`

    + ``Aao`` Reset warnings to :command:`-gnatwa`
    + ``.q`` Questionable / inneficient layout of record type
    + ``.s`` Overriden size clause (sizes mismatch)
    + ``.C`` No warning for incomplete component representation clause
    + ``I`` No warning on :ada:`with` of internal GNAT package
    + ``.V`` No info message on non-default bit-order
    + ``.X`` No warning for ``Restriction (No_Exception_Propagation)``
    + ``.Z`` No warning for ``'Size mod 'Alignment /= 0``

--------------------------------
Layout and Presentation Checks
--------------------------------

.. list-table::
   :header-rows: 1

  * - Style check

    - Behavior

  * - 1-9

    - check indentation

  * - a

    - check attribute casing

  * - b

    - check no blanks at end of lines

  * - c

    - check comment format (two spaces)

  * - C

    - check comment format (one space)

  * - d

    - check no DOS line terminators

  * - f

    - check no form feeds/vertical tabs in source

  * - h

    - check no horizontal tabs in source

  * - i

    - check if-then layout

  * - k

    - check casing rules for keywords

  * - l

    - check reference manual layout

  * - m

    - check line length <= 79 characters

  * - Mnn

    - check line length <= nn characters

  * - n

    - check casing of package Standard identifiers

  * - o

    - check subprogram bodies in alphabetical order

  * - p

    - check pragma casing

  * - r

    - check casing for identifier references

  * - S

    - check separate lines after THEN or ELSE

  * - t

    - check token separation rules

  * - u

    - check no unnecessary blank lines

---------------------------------
Layout and Presentation Example
---------------------------------

.. code:: Ada
   :number-lines: 79

  -- Procedure to find the defining name for the node
  procedure Find_Defining_Name (Node : Lal.Ada_Node'Class) is
     Parent : Lal.Ada_Node := node.Parent;
  begin
     --  Go up the tree until we find what we are looking for
     Search_Loop:
     While not Parent.Is_Null loop
        exit Search_Loop when Names.Map_Size = Natural'Last;
        if Parent.Kind = Lalco.Ada_Defining_Name then
           if Valid_Length (Qualified_Name) then
             Names.Add_Name (Qualified_Name);
           end if;
        end if;
        Parent := Parent.Parent;
     end loop Search_Loop;
  end Find_Defining_Name;

.. list-table::

  * - **Message**

    - **Caused by**

  * - obfuscate.adb:79:07: (style) space required

    - *-gnatyc*

  * - obfuscate.adb:81:32: (style) bad casing of "Node" declared at line 80

    - *-gnatyr*

  * - obfuscate.adb:84:18: (style) space required

    - *-gnatyt*

  * - obfuscate.adb:85:07: (style) reserved words must be all lower case

    - *-gnatyk*

  * - obfuscate.adb:86:57: (style) bad capitalization, mixed case required

    - *-gnatya*

  * - obfuscate.adb:89:15: (style) bad indentation

    - *-gnaty3*

--------------------------
Sound Engineering Checks
--------------------------

.. list-table::
   :header-rows: 1

  * - Style check

    - Behavior

  * - A

    - check array attribute indexes

  * - B

    - check no use of AND/OR for boolean expressions

  * - e

    - check end/exit labels present

  * - I

    - check mode in

  * - Lnn

    - check max nest level < nn

  * - O

    - check overriding indicators

  * - s

    - check separate subprogram specs present

  * - x

    - check extra parentheses around conditionals

---------------------------
Sound Engineering Example
---------------------------

.. code:: Ada
   :number-lines: 4

   package Example is
      Count : Natural;
      type Tagged_T is tagged null record;
      procedure Primitive (R : in Tagged_T);
      type Child_T is new Tagged_T with record
         Field : Natural;
      end record;
      procedure Primitive (R : in Child_T);
   end Example;

   package body Example is
      procedure Primitive (R : in Tagged_T) is
      begin
         if (Count > 0) then Count := 0; end if;
      end Primitive;
      procedure Primitive (R : in Child_T) is
      begin
         Lup :
         while (Count > 0) and (Count < 100) loop
            Count := Count + R.Field;
            exit when Count = 50;
         end loop Lup;
      end Primitive;
   end Example;

.. list-table::

  * - **Message**

    - **Caused by**

  * - examples.adb:7:32: (style) "in" should be omitted

    - *-gnatyI*

  * - examples.adb:11:07: (style) missing "overriding" indicator in declaration of "Primitive"

    - *-gnatyO*

  * - examples.adb:17:13: (style) redundant parentheses

    - *-gnatyx*

  * - examples.adb:17:30: (style) no statements may follow "then" on same line

    - *-gnatyS*

  * - examples.adb:19:07: (style) missing "overriding" indicator in body of "Primitive"

    - *-gnatyO*

  * - examples.adb:22:28: (style) "and then" required

    - *-gnatyB*

  * - examples.adb:24:13: (style) "exit Lup" required

    - *-gnatye*

------------------------
Warnings Versus Errors
------------------------

+ If you must ensure issues are caught, failing to compile is the most rigorous enforcement
+ Compiler can be told to treat warnings as errors

  + Thus code rejected at compile-time

+ Use switch :command:`-gnatwe`

  + Warnings become errors
  + Style violations become errors too
  + Warning messages still appear but no code generation

----------------------------------------------
IDE Integration (Project Properties Editor)
----------------------------------------------

.. image:: gnat_studio/menu-edit/project_properties/build-switches-ada.jpg

-----------------
Warnings Dialog
-----------------

.. image:: gnat_studio/menu-edit/project_properties/build-switches-ada-warnings.jpg

---------------------
Style Checks Dialog
---------------------

.. image:: gnat_studio/menu-edit/project_properties/build-switches-ada-style.jpg

--------------------------------------
Dialog Pop-Ups Explain Style Options
--------------------------------------

.. image:: gnat_studio/menu-edit/project_properties/build-switches-ada-style-tooltip.jpg


=============================
Language Subset Definitions
=============================

--------------------------------
Definition of Language Subsets
--------------------------------

+ Uses language-defined :ada:`pragma Restrictions`

   .. code:: Ada

      pragma Restrictions (restriction{, restriction});
      restriction ::= restriction_identifier |
                      restriction_parameter_identifier =>
                            restriction_parameter_argument

+ Provides control over many features

  + Tasking, exceptions, dispatching, code generation, elaboration, etc.

+ Benefits

  + Faster execution on compatible run-time library
  + Safer coding
  + Certification restrictions compliance
  + Compiler/target portability

+ Restrictions can also be added by setting up a runtime profile via :ada:`pragma Profile (<runtime>)` which enables all restrictions implemented in the specified runtime

-----------------------------------------
Example Restriction & Violation Message
-----------------------------------------

.. container:: latex_environment tiny

  .. code:: Ada
     :number-lines: 1

     pragma Restrictions (No_Implicit_Heap_Allocations);

     with Ada.Command_Line;
     package Lib_Level is
        -- Command_Name returns an unconstrained type
        Command_Name : constant String := Ada.Command_Line.Command_Name;
     end Lib_Level;

  ::

    lib_level.ads:6:04: error: violation of restriction "No_Implicit_Heap_Allocations" at line 1

Only happens for library level package specs, not just any package and not package bodies.

-------------------------
Restriction Identifiers
-------------------------

+ All language-defined identifiers are implemented

  + Core restrictions (see 13.12.1)
  + Real-time tasking restrictions (see D.7)
  + High integrity restrictions (see H.4)

+ GNAT defines additional restriction identifiers
+ All restrictions, both language-defined and GNAT-defined, are listed and described in the *GNAT Reference Manual*

------------------------
Restriction Categories
------------------------

+ Portability
+ Allocation
+ Access Types & Values
+ Exceptions
+ OOP
+ Tasking
+ Real-Time Programming
+ Code Generation
+ Miscellaneous
+ GNAT defines additional restrictions in all these categories

   + We examine some of them here...

----------------------------------
Applying Restriction Identifiers
----------------------------------

+ In source or in configuration file

  + Configuration file name should be specified in the GPR file

    .. code:: Ada

      package Compiler is
        for Local_Configuration_Pragmas
            use "configuration_pragmas.adc";
      end Compiler;

  + Or, if not GPR file is in use, in the default config file :filename:`gnat.adc`

.. code:: Ada

     pragma Restrictions (No_Implicit_Heap_Allocations);
     pragma Restrictions (No_Implicit_Conditionals);
     pragma Restrictions (No_Entry_Calls_In_Elaboration_Code);

+ :toolname:`GNATbind` can list all restrictions that could be applied to the code corresponding to a given ALI file

  + Via :command:`-r` switch
  + Useful for code audit, and code generation control

------------------
OOP Restrictions
------------------

+ :ada:`No_Dispatch` (RM H.4)

  + Ensures no occurrences of :ada:`T'Class` for any tagged type :ada:`T`
  + Prevents dynamic dispatching (but also other usage)

+ :ada:`No_Dispatching_Calls` (GNAT)

  + Ensures generated code involves no dispatching calls
  + Allows

    + Record extensions
    + Class-wide membership tests
    + Other class-wide features

  + Does not allow involving implicit dispatching

  + Comparable to :ada:`No_Dispatch`

    + Except allows all class-wide constructs that do not imply dispatching

------
Quiz
------

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada

        package Definition is
           type T is tagged record
              Data : Natural;
           end record;
           procedure P (X : T);
           type Dt is new T with record
              More_Data : Natural;
           end record;
           not overriding procedure Q (X : Dt);
        end Definition;

      .. code:: Ada
        :number-lines: 1

        pragma Restrictions (No_Dispatching_Calls);

        with Definition; use Definition;
        procedure Demo (O : T'Class) is
           N : Natural := O'Size;
           C : T'Class := O;
        begin
           if O in Dt'Class then
              Q (Dt (O));
           else
              P (O);
           end if;
        end Demo;

  .. container:: column

    .. container:: latex_environment footnotesize

      Which line(s) violate the restriction?

      A. 5, 6, 8, 9, 11
      B. 11
      C. :answer:`5, 6, 11`
      D. No violations

    .. container:: animate

       + Line 5 - Dispatch needed to determine size of O
       + Line 6 - Just a memory copy (no dispatching)
       + Line 8 - Membership not a dispatching call
       + Line 9 - Type conversion so no dispatching
       + Line 11 - Dispatch needed to find correct :ada:`P`

-----------------------------------------
Exceptions Restrictions Form A Spectrum
-----------------------------------------

+ :ada:`No_Exceptions` (RM H.4)

  + No raise statements and no handlers

+ :ada:`No_Exception_Handlers` (GNAT)

  + No exception handlers
  + Raised exception raised result in call to the *last chance handler*

+ :ada:`No_Exception_Propagation` (GNAT)

  + Exceptions never propagated out of subprogram
  + Handlers are allowed

    + May not contain an exception occurrence identifier

  + Handler must be in same subprogram

    +  Raise is essentially a :ada:`goto` statement

  + Any other raise statement considered unhandled

---------------------------------
No_Implicit_Conditionals (GNAT)
---------------------------------

+ Generated code does not contain any implicit conditionals

  + E.g., comparisons of composite objects (maybe)
  + E.g., the Max/Min attributes (maybe)

+ Modifies the generated code where possible, or rejects any construct that would otherwise generate an implicit conditional
+ If rejected, the programmer must make the condition explicit in the source

--------------------------
No_Implicit_Loops (GNAT)
--------------------------

+ Ensures generated code does not contain any implicit loops

  + Actual code

    .. code:: Ada

       X : array (1 .. 100) of Integer := (1, 2, others => 3);

  + Generated code

    .. code:: Ada

       x (1) := 1;
       x (2) := 2;
       k : integer := 2;
       while k  <  100 loop
          k := k + 1;
          x (k) := 3;
       end loop;

+ Modifies code generation approach where possible, or rejects construct
+ If rejected, programmer must make loop explicit
+ Can improve code performance

----------------------------------
GNAT Initialization Restrictions
----------------------------------

+ :ada:`No_Initialize_Scalars`

  + No unit in partition compiled with :ada:`pragma Initialize_Scalars`
  + Allows generation of more efficient code

+ :ada:`No_Default_Initialization`

  + Forbids any default variable initialization of any kind

  .. code:: Ada
     :number-lines: 1

    pragma Restrictions (No_Default_Initialization);
    procedure Demo is
       type Record_T is record
          Field : Integer := 42;
       end record;
       Bad  : Record_T;
       Good : Record_T := (Field => 42);

  .. container:: latex_environment tiny

    ::

      demo.adb:6:04: error: violation of restriction "No_Default_Initialization" at line 1

---------------------------------
Miscellaneous GNAT Restrictions
---------------------------------

+ :ada:`No_Direct_Boolean_Operators`

  + Short-circuit forms required everywhere
  + More restrictive than GNAT style switch

+ :ada:`No_Elaboration_Code`

  + No elaboration code is generated
  + Not the same as :ada:`pragma Preelaborate`

+ :ada:`No_Enumeration_Maps`

  + No :ada:`'Image` and :ada:`'Value` applied to enumeration types

    + No need to keep strings

  + Compare to :ada:`pragma Discard_Names`

    + Applies to enumeration types, tagged types, and exceptions

--------------------------
GNAT Stream Restrictions
--------------------------

+ :ada:`No_Stream_Optimizations`

  + Performs all I/O operations on a per-character basis

    + Rather than larger whole-array object basis

+ :ada:`No_Streams`

  + No stream objects created and no use of stream attributes
  + Less code generated
  + Worth considering if using tagged types on memory-constrained targets

------------------------
No_Finalization (GNAT)
------------------------

+ Disables features described in *Ada Reference Manual* section 7.6 plus all forms of code generation supporting them

  + Initialization as well as finalization

+ Following types are no longer controlled types

  + :ada:`Ada.Finalization.Controlled` and :ada:`Limited_Controlled`
  + Types derived from :ada:`Controlled` or :ada:`Limited_Controlled`
  + Class-wide types
  + Protected types
  + Task types
  + Array and record types with controlled components

+ Compiler no longer generates code to initialize, finalize or adjust objects

=============================
Getting Representation Info
=============================

----------------------------------------------
Traceability from Source Code to Object Code
----------------------------------------------

+ Expanded sources can be viewed

  + Shows how tasks implemented, aggregates expanded, etc.
  + Facilitates certification activities

+ Expanded code syntax described in *GNAT User's Guide*
+ Enabled via :command:`-gnatG`

  + Add :command:`-gnatL` to intersperse source lines as comments

-----------------------
Expanded Code Example
-----------------------

+ Actual code

  .. code:: Ada
     :number-lines: 1

    procedure Demo is
      X : array (1 .. 100) of Integer := (1, 2, others => 3);
    begin
      null;
    end Demo;

+ Generated code

  .. code:: Ada

    -- 1: procedure Demo is
    procedure demo is
    -- 2:    X : array (1 .. 100) of Integer := (1, 2, others => 3);
       [type demo__TxB is array (1 .. 100 range <>) of integer]
       freeze demo__TxB []
       [subtype demo__TxT1b is demo__TxB (1 .. 100)]
       freeze demo__TxT1b []
       x : array (1 .. 100) of integer;
       x (1) := 1;
       x (2) := 2;
       J6b : integer := 2;
       L7b : while J6b < 100 loop
          [constraint_error when
            J6b = 16#7FFF_FFFF#
            "overflow check failed"]
          J6b := integer'succ(J6b);
          x (J6b) := 3;
       end loop L7b;
    -- 3: begin
    begin
    -- 4:    null;
       null;
    -- 5: end Demo;
       return;
    end demo;

-------------------------------------------
See How Types and Objects Are Represented
-------------------------------------------

+ Compiler switch shows all representation aspects

  + Size in memory
  + Size required for values
  + Alignment
  + Component sizes

+ Reflects user specifications

  + Record type representation
  + Array component sizes
  + et cetera

+ Reflects compiler defaults

  + When not specified by application code

--------------------------------------
Settings for Viewing Representations
--------------------------------------

-gnatR0
   No information

-gnatR1
   Size / alignment for array and record types

-gnatR2
   Size / alignment for all types and objects

-gnatR3
   Symbolic expressions for variant record info

+ If the switch is followed by an 's' the output is to a file with the name :filename:`<file>.rep` where *<file>* is the name of the corresponding source file
+ Note :command:`-gnatR` is same as -:command:`gnatR1`

--------------------------------------
Viewing Data Representations Example
--------------------------------------

+ Performing :command:`gcc -c -gnatR3` on:

   .. code:: Ada

      package Some_Types is
         type Temperature is range -275 .. 1_000;
         type Identity is range 1 .. 127;
         type Info is record
            T  : Temperature;
            Id : Identity;
         end record;
      end Some_Types;

+ Generates:

   .. code:: Ada

      for Temperature'Object_Size use 16;
      for Temperature'Value_Size use 11;
      for Temperature'Alignment use 2;

      for Identity'Object_Size use 8;
      for Identity'Value_Size use 7;
      for Identity'Alignment use 1;

      for Info'Object_Size use 32;
      for Info'Value_Size use 24;
      for Info'Alignment use 2;
      for Info use record
         T  at 0 range  0 .. 15;
         Id at 2 range  0 ..  7;
      end record;

========================================
GNAT versus GNAT Static Analysis Suite
========================================

----------------------------
GNAT Static Analysis Suite
----------------------------

+ A static analyzer

  + Provides deep analysis prior to execution and test

+ Helps identify vulnerabilities and bugs

  + Better than the compiler
  + Better than a human!

+ Is modular and scalable

  + Can be used on an entire project or a single file
  + Can be configured to be more or less strict

+ Is flexible

  + Usable with all Ada language variants
  + Usable with other vendors' compilers

--------------------------------
Why Not Just Use the Compiler?
--------------------------------

+ The compiler does generate useful warnings

  + But :toolname:`GNAT Static Analysis Suite` far exceeds the compiler's analyses

+ :toolname:`GNAT Static Analysis Suite`

  + Does much more thorough job
  + Finds problems compiler doesn't look for

------------------------------
How Does GNAT Analysis Work?
------------------------------

+ Intraprocedural

  + Ignores interactions between caller and called subprograms

+ Flow-sensitive but path- and context-insensitive

  + Recognizes order of statements
  + Ignores effects of conditional statements
  + Ignores calling context

+ Low-noise
+ Very useful, but not complete

--------------
Flow Tracing
--------------

.. code:: Ada
   :number-lines: 1

   function Example (K : Integer) return Integer is 
      A, B, C, D : Integer;
   begin
      C := A;
      if K > 4 then
         B := 3;
      end if;
      D := B;
      return D;
   end Example;

+ Compiler results:

  ::

    example.adb:2:04: warning: variable "A" is read but never assigned [-gnatwv]

+ :toolname:`GNAT Static Analysis Suite` results

  ::

    example.adb:4:9: high: validity check: A is uninitialized here
    example.adb:8:9: medium: validity check: B might be uninitialized

---------------
Value Tracing
---------------

.. code:: Ada
   :number-lines: 1

   function Example (K : Integer) return Integer is
      A : Integer;
   begin
      A := 4;
      if A > 3 then
         A := A + 1;
      end if;
      if A > 4 then
         A := A + 1;
      end if;
      return A + K;
   end Example;

+ GNAT does only rudimentary value tracing

  + Traces constant values assigned in straight-line code with no conditions

  ::

    example.adb:5:14: warning: condition is always True

+ :toolname:`GNAT Static Analysis Suite` does full value tracing

  ::

    example.adb:5:09: warning: condition is always True 
    example.adb:8:9: medium warning: test always true because A = 5

------------------------------------------------
"Intra"procedural vs. "Inter"procedural Analysis
------------------------------------------------

.. code:: Ada
   :number-lines: 1

  function Example (K : Integer) return Integer is
     A, B, C : Integer;
     function Zero return Integer is (0);
  begin
     A := 0;
     B := K / A;
     C := B / Zero;
     return C;
  end Example;

+ GNAT only analyzes one routine at a time

  .. container:: latex_environment scriptsize

    ::

      example.adb:6:13: warning: division by zero [enabled by default]

+ :toolname:`GNAT Static Analysis Suite` does whole-program analysis

  .. container:: latex_environment scriptsize

    ::

      example.adb:6:11: high: divide by zero fails here
      example.adb:7:11: high: divide by zero fails here: requires (zero'Result) /= 0

-----------------------------------------------------------------
GNAT Static Analysis Suite's Capabilities Beyond the Compiler's
-----------------------------------------------------------------

+ Detecting race conditions in tasking code
+ Incremental analysis

  + Historical database preserves results of every run
  + Allows user to focus on new problems or compare against baseline
  + Only the changes need be analyzed

+ Contract-based Programming support

  + Can generate contracts automatically from the code
  + Can detect incorrect contracts (statically)
  + Can use existing contracts in further analysis

+ Others...

=========
Summary
=========

---------
Summary
---------

+ Compiler can generate a large number of useful warnings
+ Multiple warning categories supported

  + Layout and presentation
  + Sound engineering coding practices
  + Language subset definitions

+ See the docs: we did not examine every possibility
+ :toolname:`GNAT Static Analysis Suite` can do much better, and much more

  + And analysis is sound

+ You can use these facilities directly but you can also apply them via :toolname:`GNATcheck`

