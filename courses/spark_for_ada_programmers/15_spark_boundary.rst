****************
SPARK Boundary
****************

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

==============
Introduction
==============

----------------------
Modelling the System
----------------------

* Special variables used to interact with the system

  - Usually marked as volatile for the compiler
  - This prevents compiler optimizations

|

* :toolname:`GNATprove` needs to model these interactions

  - Both in flow analysis and proof
  - Distinction between different kinds of interactions

|

* This modelling is used as assumptions by :toolname:`GNATprove`

  - These assumptions need to be reviewed

------------------------
Integrating SPARK Code
------------------------

* Not all the program is in SPARK usually

  - The Operating System (if present) is rarely in SPARK
  - Some services (logging, input/output) may not be in SPARK
  - Only a core part may be in SPARK

|

* User needs to specify the boundary of SPARK code

|

* :toolname:`GNATprove` needs to model interactions with non-SPARK code

|

* GNAT needs to compile SPARK and non-SPARK code together

=================
System Boundary
=================

--------------------------
Volatile Variables (1/2)
--------------------------

* Volatile variable is identified by aspect :ada:`Volatile`

  - Either on the variable or its type
  - Aspect :ada:`Atomic` implies :ada:`Volatile`

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
       Address =>
         System.Storage_Elements.To_Address (16#CAFECAFE#);

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

  - :ada:`Async_Readers` - asynchronous reader may read the variable
  - :ada:`Async_Writers` - asynchronous write may write to the variable
  - :ada:`Effective_Reads` - reading the variable changes its value
  - :ada:`Effective_Writes` - writing the variable changes its value

|

* Each is a Boolean aspect of volatile variables

  - By default a volatile variable has all four set to :ada:`True`
  - When one or more are set explicitly, others default to :ada:`False`

----------------------------------
Volatility Properties - Examples
----------------------------------

* A sensor (program input) has aspect

  - :ada:`Async_Writers => True`

|

* An actuator (program output) has aspect

  - :ada:`Async_Readers => True`

|

* A machine register (single data) has aspects

  - :ada:`Effective_Reads => False`
  - :ada:`Effective_Writes => False`

|

* A serial port (stream of data) has aspects

  - :ada:`Effective_Reads => True`
  - :ada:`Effective_Writes => True`

--------------------
Volatile Functions
--------------------

* Some volatile variables can be read in functions

  - When :ada:`Async_Writers` and :ada:`Effective_Reads` are set to :ada:`False`
  - These correspond to program outputs

* :dfn:`Volatile functions` can read volatile inputs

  - When :ada:`Async_Writers` is set to :ada:`True`
  - Function needs to have the aspect :ada:`Volatile_Function`

* Functions (even volatile ones) cannot read some volatile variables

  - When :ada:`Effective_Reads` is set to :ada:`True`
  - A read is a side-effect, which is forbidding in SPARK functions
  - Unless the function has aspect :ada:`Side_Effects`

* A call to a volatile function must appear in a non-interfering context

  - Same as a read of a volatile variable

----------------
External State
----------------

* Abstract state may have volatile variables as constituents

  - Abstract state needs to have aspect :ada:`External`

|

* An external state is subject to the four volatility properties

  - All volatility properties set to :ada:`True` by default
  - Specific properties can be specified like for volatile variables
  - An external state with :ada:`Prop` set to :ada:`False` can only have

    + Non-volatile constituents
    + Volatile constituents with :ada:`Prop` set to :ada:`False`

|

* Special case for external state always initialized

  - An external state with :ada:`Async_Writers` set to :ada:`True`
  - The asynchronous writer is responsible for initialization

---------------------------------------
Effect of Volatility on Flow Analysis
---------------------------------------

* A variable with :ada:`Effective_Reads` set to :ada:`True`

  - Has its value influenced by conditions on branches where read happens

  .. code:: ada

     Var : Integer := 42 with Volatile, Effective_Reads;
     if Cond then
        Val := Var;
     end if;
     -- value of Var here depends on Cond

* A variable with :ada:`Effective_Writes` set to :ada:`True`

  - Never triggers a warning on unused assignment

  .. code:: ada

     Var : Integer := 42 with Volatile, Effective_Writes;
     Var := 1; -- previous assignment is not useless

-------------------------------
Effect of Volatility on Proof
-------------------------------

* A variable is :dfn:`effectively volatile for reading` if

  - It has :ada:`Async_Writers` set to :ada:`True`
  - Or it has :ada:`Effective_Reads` set to :ada:`True`

* The value of such a variable is never known

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

* SPARK code is identified by pragma/aspect :ada:`SPARK_Mode` with value
  :ada:`On`

|

* Other values: :ada:`Off` or :ada:`Auto`

  - :ada:`Off` to exclude code
  - :ada:`Auto` to include only SPARK-compatible declarations (not bodies)

|

* Default is :ada:`On` when using :ada:`SPARK_Mode` without value

|

* Default is :ada:`Auto` when :ada:`SPARK_Mode` not specified

  - :ada:`Auto` can only be used explicitly in configuration pragmas

---------------------------------
Sections with :ada:`SPARK_Mode`
---------------------------------

* Subprograms can have 1 or 2 sections: spec and body

  - :ada:`SPARK_Mode` can be :ada:`On` for spec then :ada:`On` or
    :ada:`Off` for body

|

* Packages can have between 1 and 4 sections:

  - package spec visible and private parts, package body declarations and
    statements
  - :ada:`SPARK_Mode` can be :ada:`On` for some sections then :ada:`On` or
    :ada:`Off` for the remaining sections

|

* :ada:`SPARK_Mode` **cannot** be :ada:`Off` for a section

  - Then :ada:`On` for a following section
  - Or :ada:`On` inside the section

-------------------------------------------------
Inheritance for :ada:`SPARK_Mode` on Subprogram
-------------------------------------------------

* Value of :ada:`SPARK_Mode` inherited inside subprogram body

  - Nested subprogram or package can have :ada:`SPARK_Mode` with value
    :ada:`Off`

|

* Value for subprogram spec **not** inherited for subprogram body

----------------------------------------------
Inheritance for :ada:`SPARK_Mode` on Package
----------------------------------------------

* Value :ada:`On` of :ada:`SPARK_Mode` inherited inside package spec/body

  - Nested subprogram or package can have :ada:`SPARK_Mode` with value
    :ada:`Off`

* Value :ada:`Off` of :ada:`SPARK_Mode` inherited inside package spec/body

* Value :ada:`Auto` of :ada:`SPARK_Mode` inherited inside package spec/body

  - Nested subprogram or package can have :ada:`SPARK_Mode` with value
    :ada:`On` or :ada:`Off`

* Value for package spec visible part inherited in private part

* Value for package body declarations inherited for body statements

* Value for package spec **not** inherited for package body

-------------------------------
Syntax for :ada:`SPARK_Mode`
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
Generics and :ada:`SPARK_Mode`
---------------------------------

* Remember: only generic instances are analyzed

* If generic spec/body has no value of :ada:`SPARK_Mode`

  - Each instance spec/body inherites value from context
  - As if the instantiation was replaced by the instance spec and body

* If generic spec/body has :ada:`SPARK_Mode` with value :ada:`On`

  - Each instance spec/body has :ada:`SPARK_Mode` with value :ada:`On`
  - Unless context has value :ada:`Off`, which takes precedence

    + Remember: :ada:`SPARK_Mode` **cannot** be :ada:`Off` then :ada:`On`

* If generic spec/body has :ada:`SPARK_Mode` with value :ada:`Off`

  - Each instance spec/body has :ada:`SPARK_Mode` with value :ada:`Off`

* Value of library-level pragma inside generic file **not** inherited in
  instance

-------------------
Typical Use Cases
-------------------

* Unit fully in SPARK

  - Spec and body both have :ada:`SPARK_Mode` with value :ada:`On`

* Spec only in SPARK

  - Spec has :ada:`SPARK_Mode` with value :ada:`On`
  - Body has no :ada:`SPARK_Mode` or with value :ada:`Off`

* Package spec is partly in SPARK

  - Visible part of spec has :ada:`SPARK_Mode` with value :ada:`On`
  - Private part of spec has :ada:`SPARK_Mode` with value :ada:`Off`
  - Body has no :ada:`SPARK_Mode` or with value :ada:`Off`

* Package is partly in SPARK

  - Spec and body both have :ada:`SPARK_Mode` with value :ada:`On`
  - Some subprograms inside have :ada:`SPARK_Mode` with value :ada:`Off` on
    spec and body

------------------------------
Multiple Levels of Use (1/2)
------------------------------

* Level 1: :ada:`SPARK_Mode` as a configuration pragma

* :ada:`SPARK_Mode` can be specified in a global/local configuration pragmas
  file

  - Configuration pragmas file referenced in the GNAT project file
  - Only for :ada:`SPARK_Mode` with value :ada:`On`

* :ada:`SPARK_Mode` can be specified as library-level pragma in a file

  - Initial pragmas in a file before with/use clauses
  - Takes precedence over value in configuration pragmas file
  - Typically for :ada:`SPARK_Mode` with value :ada:`On` or :ada:`Off`
  - Can be used with explicit value :ada:`Auto`

    + Useful when configuration pragmas file has value :ada:`On`

------------------------------
Multiple Levels of Use (2/2)
------------------------------

* Level 2: :ada:`SPARK_Mode` as a program unit pragma

* :ada:`SPARK_Mode` can be specified on top-level subprogram or package

  - Takes precedence over value in library-level pragmas
  - Only for :ada:`SPARK_Mode` with value :ada:`On` or :ada:`Off`

* :ada:`SPARK_Mode` can be specified on nested subprogram or package

  - Takes precedence over inherited value from context
  - Only for :ada:`SPARK_Mode` with value :ada:`On` or :ada:`Off`

--------------------------------
Integrating SPARK and Ada Code
--------------------------------

* SPARK code has :ada:`SPARK_Mode` with value :ada:`On`

|

* Ada code has no :ada:`SPARK_Mode` or with value :ada:`Off`

|

* GNAT compiles all code together

|

* Contracts on Ada subprograms must be correct

  - As if the subprogram was implemented in SPARK
  - Precondition must prevent RTE in subprogram (for Silver level and above)
  - Postcondition must be respected by subprogram
  - Data dependencies must be either generated or accurate

    + This may require introducing abstract states for Ada units

------------------------------------
Integrating SPARK and C Code (1/2)
------------------------------------

* GNAT data layout follows C ABI by default

  - Representation clauses may change the default
  - Aspect :ada:`Pack` forces data packing

* Subprograms used across the boundary

  - Must have aspect :ada:`Convention => C`
  - Must be marked with aspect :ada:`Import` or :ada:`Export`
  - Must have their C name given in aspect :ada:`External_Name`

* Parameters of these subprograms

  - Ada mode :ada:`in out` |rightarrow| C pointer
  - Ada record/array |rightarrow| C pointer
  - Ada scalar |rightarrow| C scalar

------------------------------------
Integrating SPARK and C Code (2/2)
------------------------------------

* Standard library units

  - :ada:`Interfaces` defines fixed-size scalar types
  - :ada:`Interfaces.C` defines C standard scalar types
  - :ada:`Interfaces.C.Strings` defines character and string conversion
    functions between Ada and C

* SPARK Library units    

  - :ada:`SPARK.C.Strings` defines wrapper on :ada:`Interfaces.C.Strings` for
    mutable strings based on ownership
  - :ada:`SPARK.C.Constant_Strings` defines wrapper on
    :ada:`Interfaces.C.Strings` for read-only strings (aliasing **is** allowed)
  
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

--------------------------------------------
Integrating with Main Procedure Not in Ada
--------------------------------------------

* GNAT compiler generates startup and closing code

  - Procedure :ada:`adainit` calls elaboration code
  - Procedure :ada:`adafinal` calls finalization code
  - These are generated in the file generated by :toolname:`GNATbind`

* When using a main procedure not in Ada

  - Main procedure should declare :ada:`adainit` and :ada:`adafinal`

    .. code:: ada

       extern void adainit (void);
       extern void adafinal (void);

  - Main procedure should call :ada:`adainit` and :ada:`adafinal`

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

|

* Relevant global data should be modelled

  - As abstract states when not accessed concurrently
  - As external states when accessed concurrently

|

* API subprogram contracts model actual behavior

  - Data dependencies must reflect effects on global data
  - Functional contracts can model underlying automatons

    + Possibly defining ghost query functions, e.g. :ada:`Is_Open` for a file
    + Ghost function may be marked :ada:`Import` when not implementable

----------------------------
Modelling an API - Example
----------------------------

* Standard unit :ada:`Ada.Text_IO` is modelled in SPARK

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
	Exceptional_Cases =>
	  (Name_Error | Use_Error => Standard.True);

      function Is_Open (File : File_Type) return Boolean with
        Global => null;

---------------------------------------
Modelling an API to Manage a Resource
---------------------------------------

* Managing a resource may require

  - Preventing aliasing of the resource

    + e.g. with limited type as in :ada:`Ada.Text_IO.File_Type`

  - Requiring release of the resource

    + e.g. free memory, close file or socket, ...

* :toolname:`GNATprove` can force ownership on a type

  - With :ada:`Annotate => (GNATprove, Ownership)`

    + On a private type
    + When private part of package has :ada:`SPARK_Mode` with value :ada:`Off`

  - Assignment transfers ownership of object

    + Similar to treatment of pointers in SPARK
    + :toolname:`GNATprove` checks absence of aliasing

  - Possibility to specify a reclamation function, predicate, or value

    + :toolname:`GNATprove` checks absence of resource leaks

-------------------------------------------------
Modelling an API to Manage a Resource - Example
-------------------------------------------------

.. code:: ada

   package Text_IO with
     SPARK_Mode,
     Always_Terminates
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

   * No - :toolname:`GNATprove` assumes that :ada:`Random` is a mathematical
     function

     - An abstract state should be added in package :ada:`Random_Numbers`
     - :ada:`Random` should be a procedure
     - A data dependency contract should be added for reads/writes to this
       abstract state

   * No - :toolname:`GNATprove` assumes that the postcondition of :ada:`Random`
     is always satisfied, even when :ada:`From > To`

     - A precondition :ada:`From <= To` should be added
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

* SPARK software boundary defined by aspect/pragma :ada:`SPARK_Mode`

  - Fine-grain integration of SPARK and non-SPARK code is possible

* Integration with other programming languages

  - Easiest between SPARK and Ada
  - Easy between SPARK and C
  - Usually based on C integration for other languages

* Formal verification is based on assumptions

  - Assumptions at the boundary need to be reviewed
