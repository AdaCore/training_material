****************
SPARK Boundary
****************

..
    Coding language

.. role:: ada(code)
    :language: Ada

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

----------------------
Modelling the System
----------------------

* Special variables used to interact with the system

  - Usually marked as volatile for the compiler
  - This prevents compiler optimizations

* :toolname:`GNATprove` needs to model these interactions

  - Both in flow analysis and proof
  - Distinction between different kinds of interactions

* This modelling is used as assumptions by :toolname:`GNATprove`

  - These assumptions need to be reviewed

------------------------
Integrating SPARK Code
------------------------

* Not all the program is in SPARK usually

  - The Operating System (if present) is rarely in SPARK
  - Some services (logging, input/output) may not be in SPARK
  - Only a core part may be in SPARK

* User needs to specify the boundary of SPARK code

* :toolname:`GNATprove` needs to model interactions with non-SPARK code

* GNAT needs to compile SPARK and non-SPARK code together

=================
System Boundary
=================

--------------------------
Volatile Variables (1/2)
--------------------------

* Volatile variable is identified by aspect :code:`Volatile`

  - Either on the variable or its type
  - Aspect :code:`Atomic` implies :code:`Volatile`

* :toolname:`GNATprove` assumes that volatile variable may change value

  - Each read gives a different value
  - Even if read is preceded by a write

.. code:: ada

   Var  : Integer := 42 with Volatile;
   Val1 : Integer := Var;
   Val2 : Integer := Var;
   pragma Assert (Val1 = 42);   -- unprovable
   pragma Assert (Val1 = Val2); -- unprovable

--------------------------
Volatile Variables (2/2)
--------------------------

* Volatile variable typically has its address specified

  .. code:: ada

     Var : T with
       Volatile,
       Address => System.Storage_Elements.To_Address (16#CAFECAFE#);

* A volatile variable can only occur in a :dfn:`non-interfering context`

  - On either side of an assignment

    + As whole variable or as prefix when accessing a component

  - But not as part of a more complex expression

  .. code:: ada

     Var := Var + 1; -- illegal

     Tmp : Integer := Var;
     Var := Tmp + 1; -- legal

-----------------------
Volatility Properties
-----------------------

* Four different properties of volatile variables in SPARK

  - :code:`Async_Readers` - asynchronous reader may read the variable
  - :code:`Async_Writers` - asynchronous write may write to the variable
  - :code:`Effective_Reads` - reading the variable changes its value
  - :code:`Effective_Writes` - writing the variable changes its value

* Each is a Boolean aspect of volatile variables

  - By default a volatile variable has all four set to :code:`True`
  - When one or more are set explicitly, others default to :code:`False`

* A sensor has aspect :code:`Async_Writers => True`

  - It is a program input

* An actuator has aspect :code:`Async_Readers => True`

  - It is a program output

* A machine register has :code:`Effective_Reads` and :code:`Effective_Writes`
  set to :code:`False`

  - It is a single data

* A serial port has :code:`Effective_Reads` and :code:`Effective_Writes` set to
  :code:`True`

  - It is a stream of data

--------------------
Volatile Functions
--------------------

* Some volatile variables can be read in functions

  - When :code:`Async_Writers` and :code:`Effective_Reads` are set to :code:`False`
  - These correspond to program outputs

* :dfn:`Volatile functions` can read volatile inputs

  - When :code:`Async_Writers` is set to :code:`True`
  - Function needs to have the aspect :code:`Volatile_Function`

* Functions (even volatile ones) cannot read some volatile variables

  - When :code:`Effective_Reads` is set to :code:`True`
  - A read is a side-effect, which is forbidding in SPARK functions

* A call to a volatile function must appear in a non-interfering context

  - Same as a read of a volatile variable

----------------
External State
----------------

* Abstract state may have volatile variables as constituents

  - Abstract state needs to have aspect :code:`External`

* An external state is subject to the four volatility properties

  - All volatility properties set to :code:`True` by default
  - Specific properties can be specified like for volatile variables
  - An external state with :code:`Prop` set to :code:`False` can only have

    + Non-volatile constituents
    + Volatile constituents with :code:`Prop` set to :code:`False`

* Special case for external state always initialized

  - An external state with :code:`Async_Writers` set to :code:`True`
  - The asynchronous writer is responsible for initialization

---------------------------------------
Effect of Volatility on Flow Analysis
---------------------------------------

* A variable with :code:`Effective_Reads` set to :code:`True`

  - Has its value influenced by conditions on branches where read happens

  .. code:: ada

     Var : Integer := 42 with Volatile, Effective_Reads;
     if Conf then
        Val := Var;
     end if;
     -- value of Var here depends on Cond

* A variable with :code:`Effective_Writes` set to :code:`True`

  - Never triggers a warning on unused assignment

  .. code:: ada

     Var : Integer := 42 with Volatile, Effective_Writes;
     Val := 1; -- previous assignment is not useless

-------------------------------
Effect of Volatility on Proof
-------------------------------

* A variable is :dfn:`effectively volatile for reading` if

  - It has :code:`Async_Writers` set to :code:`True`
  - Or it has :code:`Effective_Reads` set to :code:`True`

* The value of such a variable a variable is never known

* Same for external state with these volatility properties

.. code:: ada

   Var : Integer := 42 with Volatile, Async_Readers;
   pragma Assert (Var = 42); -- proved

   Var : Integer := 42 with Volatile, Async_Writers;
   Val : Integer := Var;
   pragma Assert (Val = 42); -- unprovable

===================
Software Boundary
===================

------------------------
Identifying SPARK Code
------------------------

* SPARK code is identified by pragma/aspect :code:`SPARK_Mode` with value
  :code:`On`

  - Other values: :code:`Off` or :code:`Auto`

    + :code:`Off` to exclude code
    + :code:`Auto` to include only SPARK-compatible declarations (not bodies)

  - Default is :code:`On` when using :code:`SPARK_Mode` without value
  - Default is :code:`Auto` when :code:`SPARK_Mode` not specified

* Subprograms can have 1 or 2 sections: spec and body

  - :code:`SPARK_Mode` can be :code:`On` for spec then :code:`On` or
    :code:`Off` for body

* Packages can have between 1 and 4 sections:

  - package spec visible and private parts, package body declarations and
    statements
  - :code:`SPARK_Mode` can be :code:`On` for some sections then :code:`On` or
    :code:`Off` for the remaining sections

* :code:`SPARK_Mode` **cannot** be :code:`Off` for a section

  - Then :code:`On` for a following section
  - Or :code:`On` inside the section

------------------------------------
Inheritance for :code:`SPARK_Mode`
------------------------------------

* Value of :code:`SPARK_Mode` inherited inside subprogram body

  - Nested subprogram or package can have :code:`SPARK_Mode` with value
    :code:`Off`

* Value for subprogram spec **not** inherited for subprogram body

* Value :code:`On` of :code:`SPARK_Mode` inherited inside package spec/body

  - Nested subprogram or package can have :code:`SPARK_Mode` with value
    :code:`Off`

* Value :code:`Off` of :code:`SPARK_Mode` inherited inside package spec/body

* Value :code:`Auto` of :code:`SPARK_Mode` inherited inside package spec/body

  - Nested subprogram or package can have :code:`SPARK_Mode` with value
    :code:`On` or :code:`Off`

* Value for package spec visible part inherited in private part

* Value for package body declarations inherited for body statements

* Value for package spec **not** inherited for package body

-------------------------------
Syntax for :code:`SPARK_Mode`
-------------------------------

* Aspect on declarations (pragma is also possible)

* Pragma in other cases

.. code:: ada

   pragma SPARK_Mode; -- library-level pragma

   with Lib; use Lib;

   package P
     with SPARK_Mode -- aspect on declaration
   is
      ...
      procedure Proc
        with SPARK_Mode => Off; -- aspect on declaration
      ...
   private
      pragma SPARK_Mode (Off); -- pragma for private part
      ...
   end P;

---------------------------------
Generics and :code:`SPARK_Mode`
---------------------------------

* Remember: only generic instances are analyzed

* If generic spec/body has no value of :code:`SPARK_Mode`

  - Each instance spec/body inherites value from context
  - As if the instantiation was replaced by the instance spec and body

* If generic spec/body has :code:`SPARK_Mode` with value :code:`On`

  - Each instance spec/body has :code:`SPARK_Mode` with value :code:`On`
  - Unless context has value :code:`Off`, which takes precedence

    + Remember: :code:`SPARK_Mode` **cannot** be :code:`Off` then :code:`On`

* If generic spec/body has :code:`SPARK_Mode` with value :code:`Off`

  - Each instance spec/body has :code:`SPARK_Mode` with value :code:`Off`

* Value of library-level pragma inside generic file **not** inherited in
  instance

-------------------
Typical Use Cases
-------------------

* Unit fully in SPARK

  - Spec and body both have :code:`SPARK_Mode` with value :code:`On`

* Spec only in SPARK

  - Spec has :code:`SPARK_Mode` with value :code:`On`
  - Body has no :code:`SPARK_Mode` or with value :code:`Off`

* Package spec is partly in SPARK

  - Visible part of spec has :code:`SPARK_Mode` with value :code:`On`
  - Private part of spec has :code:`SPARK_Mode` with value :code:`Off`
  - Body has no :code:`SPARK_Mode` or with value :code:`Off`

* Package is partly in SPARK

  - Spec and body both have :code:`SPARK_Mode` with value :code:`On`
  - Some subprograms inside have :code:`SPARK_Mode` with value :code:`Off` on
    spec and body

------------------------
Multiple Levels of Use
------------------------

* :code:`SPARK_Mode` can be specified in a global/local configuration pragmas
  file

  - Configuration pragmas file referenced in the GNAT project file
  - Only for :code:`SPARK_Mode` with value :code:`On`

* :code:`SPARK_Mode` can be specified as library-level pragma in a file

  - Initial pragmas in a file before with/use clauses
  - Takes precedence over value in configuration pragmas file
  - Typically for :code:`SPARK_Mode` with value :code:`On` or :code:`Off`
  - Can be used with explicit value :code:`Auto`

    + Useful when configuration pragmas file has value :code:`On`

* :code:`SPARK_Mode` can be specified on top-level subprogram or package

  - Takes precedence over value in library-level pragmas
  - Only for :code:`SPARK_Mode` with value :code:`On` or :code:`Off`

* :code:`SPARK_Mode` can be specified on nested subprogram or package

  - Takes precedence over inherited value from context
  - Only for :code:`SPARK_Mode` with value :code:`On` or :code:`Off`

--------------------------------
Integrating SPARK and Ada Code
--------------------------------

* SPARK code has :code:`SPARK_Mode` with value :code:`On`

* Ada code has no :code:`SPARK_Mode` or with value :code:`Off`

* GNAT compiles all code together

* Contracts on Ada subprograms must be correct

  - As if the subprogram was implemented in SPARK
  - Precondition must prevent RTE in subprogram (for Silver level and above)
  - Postcondition must be respected by subprogram
  - Data dependencies must be either generated or accurate

    + This may require introducing abstract states for Ada units

------------------------------
Integrating SPARK and C Code
------------------------------

* GNAT data layout follows C ABI by default

  - Representation clauses may change the default
  - Aspect :code:`Pack` forces data packing

* Subprograms used across the boundary

  - Must have aspect :code:`Convention => C`
  - Must be marked with aspect :code:`Import` or :code:`Export`
  - Must have their C name given in aspect :code:`External_Name`

* Parameters of these subprograms

  - Ada mode :code:`in out` |rightarrow| C pointer
  - Ada record/array |rightarrow| C pointer
  - Ada scalar |rightarrow| C scalar

* Standard library unit

  - :code:`Interfaces.C` defines C standard scalar types
  - :code:`Interfaces.C.Strings` defines character and string conversion
    functions between Ada and C

---------------------------------------------------
Integrating SPARK and Other Programming Languages
---------------------------------------------------

* Based on integration of Ada with other languages

  - Standard support for COBOL and Fortran
  - GNAT specific backends for Java and .NET
  - Based on C integration for C++, Rust, Python...

* C-Based Integration

  - Same as for integrating with C code on both sides
  - Use same external name (no mangling)

* Thin binding and thick binding

  - :dfn:`Thin binding` matches closely constructs at C level
  - :dfn:`Thick binding` matches SPARK semantics
  - It is common to have both

    + Thin binding may be auto-generated (e.g. using :command:`gcc
      -fdump-ada-spec`)
    + Thick binding defines wrappers around thin binding
    + Function with side-effects in thin binding |rightarrow| procedure in
      thick binding

--------------------------------------------
Integrating With Main Procedure not in Ada
--------------------------------------------

* GNAT compiler generates startup and closing code

  - Procedure :code:`adainit` calls elaboration code
  - Procedure :code:`adafinal` calls finalization code
  - These are generated in the file generated by :toolname:`GNATbind`

* When using a main procedure not in Ada

  - Main procedure should declare :code:`adainit` and :code:`adafinal`

    .. code:: ada

       extern void adainit (void);
       extern void adafinal (void);

  - Main procedure should call :code:`adainit` and :code:`adafinal`

* When generating a stand-alone library

  - Specify interface units with :code:`Library_Interface` in project file
  - GNAT then generates library initialization code

    + This code is executed at library loading (depends on platform support)

------------------
Modelling an API
------------------

* API may be modelled in SPARK

  - Implementation may be in Ada, C, Rust...
  - Implementation may be in the Operating System

* Relevant global data should be modelled

  - As abstract states when not accessed concurrently
  - As external states when accessed concurrently

* API subprogram contracts model actual behavior

  - Data dependencies must reflect effects on global data
  - Functional contracts can model underlying automatons

    + Possibly defining ghost query functions, e.g. :code:`Is_Open` for a file
    + Ghost function may be marked :code:`Import` when not implementable

----------------------------
Modelling an API - Example
----------------------------

* Standard unit :code:`Ada.Text_IO` is modelled in SPARK

  - Subprograms can be called in SPARK code
  - File system is not precisely modelled

.. code:: ada

   package Ada.Text_IO with
     SPARK_Mode,
     Abstract_State => File_System,
     Initializes    => File_System,
   is
      type File_Type is limited private with
        Default_Initial_Condition => (not Is_Open (File_Type));

      procedure Create (File : in out File_Type; ...)
      with
        Pre      => not Is_Open (File),
        Post     => Is_Open (File) and then ...
        Global   => (In_Out => File_System),
        Annotate => (GNATprove, Might_Not_Return);

      function Is_Open (File : File_Type) return Boolean with
        Global   => null,
        Annotate => (GNATprove, Always_Return);

---------------------------------------
Modelling an API to Manage a Resource
---------------------------------------

* Managing a resource may require

  - Preventing aliasing of the resource

    + E.g. with limited type as in :code:`Ada.Text_IO.File_Type`

  - Requiring release of the resource

    + E.g. free memory, close file or socket, ...

* :toolname:`GNATprove` can force ownership on a type

  - With :code:`Annotate => (GNATprove, Ownership)`

    + On a private type
    + In a package spec whose private part has :code:`SPARK_Mode` with value
      :code:`Off`

  - Assignment transfers ownership of object

    + Similar to treatment of pointers in SPARK
    + :toolname:`GNATprove` checks absence of aliasing

  - Possibility to specify a reclamation function or predicate

    + :toolname:`GNATprove` checks absence of resource leaks

-------------------------------------------------
Modelling an API to Manage a Resource - Example
-------------------------------------------------

.. code:: ada

   package Text_IO with
     SPARK_Mode,
     Annotate => (GNATprove, Always_Return)
   is
      type File_Descriptor is limited private with
        Default_Initial_Condition => not Is_Open (File_Descriptor),
        Annotate => (GNATprove, Ownership, "Needs_Reclamation");

      function Is_Open (F : File_Descriptor) return Boolean with
        Global => null,
        Annotate => (GNATprove, Ownership, "Needs_Reclamation");

      function Open (N : String) return File_Descriptor with
        Global => null,
        Post => Is_Open (Open'Result);

      procedure Close (F : in out File_Descriptor) with
        Global => null,
        Post => not Is_Open (F);
   private
      pragma SPARK_Mode (Off);
      type Text;
      type File_Descriptor is access all Text;
   end Text_IO;

=============
Assumptions
=============

-----------------------------
Quiz - Implicit Assumptions
-----------------------------

Is the following code correct?

.. code:: ada

   package Random_Numbers
     with SPARK_Mode
   is
      function Random (From, To : Integer) return Integer
        with Post => Random'Result in From .. To;
   private
      pragma SPARK_Mode (Off);
      ...

.. container:: animate

   * No - :toolname:`GNATprove` assumes that :code:`Random` is a mathematical
     function

     - An abstract state should be added in package :code:`Random_Numbers`
     - :code:`Random` should be a procedure
     - A data dependency contract should be added for reads/writes to this
       abstract state

   * No - :toolname:`GNATprove` assumes that the postcondition of :code:`Random`
     is always satisfied, even when :code:`From > To`

     - A precondition :code:`From <= To` should be added
     - The implementation must satisfy the postcondition

------------------
Tool Assumptions
------------------

* Results of flow analysis and proof are valid under assumptions

  - About the system behavior as modelled in SPARK
  - About parts of the code not in SPARK
  - About the hardware platform

* All assumptions should be reviewed and validated

  - Complete list in SPARK User's Guide section 7.3.7

* Common assumptions whether or not complete program in SPARK

* Additional assumptions

  - When only part of the program in SPARK
  - When :toolname:`GNATprove` never called with all bodies available
  - When code not compiled with GNAT

=====
Lab
=====

.. include:: labs/15_spark_boundary.lab.rst

=========
Summary
=========

----------------
SPARK Boundary
----------------

* System (hardware, OS) can be modelled in SPARK

  - Using volatile variables and external states
  - With precise volatility properties

* SPARK software boundary defined by aspect/pragma :code:`SPARK_Mode`

  - Fine-grain integration of SPARK and non-SPARK code is possible

* Integration with other programming languages

  - Easiest between SPARK and Ada
  - Easy between SPARK and C
  - Usually based on C integration for other languages

* Formal verification is based on assumptions

  - Assumptions at the boundary need to be reviewed
