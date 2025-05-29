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
Modeling an API
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
Modeling an API - Example
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
Modeling an API to Manage a Resource
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
Modeling an API to Manage a Resource - Example
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

