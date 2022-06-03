***************
GNAT Warnings
***************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==========
Warnings
==========

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

=================
Definite Errors
=================

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

=================
Probable Errors
=================

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

* Line 4 - Coder probably meant :ada:`2 ** 32`

   * But maybe not? It could be a bit location

* Line 11 - :ada:`E` is :ada:`natural`, so it can never be less than zero (without invalid data)

* Line 13 - :ada:`D` is an :ada:`out` parameter, so there is no guarantee on it's initial value

* Line 22 - Did you mean :ada:`-(D mod B)` or :ada:`(-D) mod B`?

================
Redundant Code
================

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

=================
Common Warnings
=================

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

---------------------------------------------------
Highly Optional Warnings Enabled By -gnatw.e
---------------------------------------------------

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
+ Warnings Off pragmas (flags unnecessary pragmas)
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
         Put_Line (A'image);
      end if;
   end Test;

.. container:: latex_environment tiny

  ::

    examples.adb:6:10: warning: this code can never be executed and has been deleted [-gnatwt]

==========================
Embedded Warning Control
==========================

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

=====
Lab
=====

.. include:: labs/020_gnat_warnings.lab.rst
