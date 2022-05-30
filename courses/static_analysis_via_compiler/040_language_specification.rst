************************
Language Specification
************************

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

============================
Language Subset Definitions
============================

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

+ Restrictions can also be added by setting up a runtime profile via :ada:`Pragma Profile(<runtime>)` which enables all restrictions implemented in the specified runtime

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
    + Classwide membership tests
    + Other classwide features

  + Does not allow involving implicit dispatching

  + Comparable to :ada:`No_Dispatch`

    + Except allows all classwide constructs that do not imply dispatching

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
        procedure Demo (O : T'class) is
           N : Natural := O'size;
           C : T'class := O;
        begin
           if O in Dt'class then
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
