
*******************
Program Structure
*******************

==============
Introduction
==============

--------------
Introduction
--------------

* Moving to "bigger" issues of overall program composition
* How to compose programs out of program units
* How to control object lifetimes
* How to define subsystems
* Et cetera

===============
Library Units
===============

----------
Examples
----------

.. include:: examples/130_program_structure/library_units.rst

---------------
Library Units
---------------

* Those not nested within another program unit
* Candidates

   - Subprograms
   - Packages
   - Generic Units
   - Generic Instantiations
   - Renamings

* Restrictions

   - No library level tasks 

      + They are always nested within another unit

   - No overloading at library level
   - No library level functions named as operators

---------------
Library Units
---------------

.. code:: Ada

   package Operating_System is
     procedure Foo( ... );
     procedure Bar( ... );
     package Process_Manipulation is
       ...
     end Process_Manipulation;
     package File_System is
       ...
     end File_System;
   end Operating_System;
 
* `Operating_System` is library unit
* `Foo`, `Bar`, etc - not library units

---------------------------
No 'Object' Library Items
---------------------------

.. code:: Ada

   package P is
     ...
   end P;
   
   X : Integer; -- no such thing as "file scope"
   
   procedure Bar;
   
   procedure Z (Formal : in out Integer) is
     Local : Integer;
   begin
     ...
   end Z;
   
   function Foo (This : Float) return Float;
 
-----------------------------
Declared Object "Lifetimes"
-----------------------------

.. container:: columns

 .. container:: column
  
    * Same as their enclosing declarative region

       - Objects are always declared within some declarative region

    * No ``static`` etc. directives as in C
    * Example: declared within any subprogram

       - Exist only while subprogram executes

 .. container:: column
  
    .. code:: Ada
    
       procedure P is
         X : Integer;
         Y : Real;
       begin
         ...
       end P;
     
--------------------
Program "Lifetime"
--------------------

* Run-time library is initialized
* All (any) library packages are elaborated

   - Declarations in package declarative part are elaborated
   - Declarations in package body declarative part are elaborated
   - Executable part of package body is executed (if present)

* Main program's declarative part is elaborated
* Main program's sequence of statements executes
* Program executes until all threads terminate
* All objects in library packages cease to exist
* Run-time library shuts down

-----------------------------
Objects In Library Packages
-----------------------------

* Exist as long as program executes (i.e., "forever")

.. code:: Ada

   package Named_Common is  
     X : Integer; -- valid object for life of application
     Y : Real;    -- valid object for life of application
   end Named_Common;
 
---------------------------------
Objects In Non-library Packages
---------------------------------

* Exist as long as region enclosing the package

.. code:: Ada

   procedure P is
     X : Integer; -- available while in P and Inner
     package Inner is
       Z : Boolean; -- available while in Inner
     end Inner;
     Y : Real; -- available while in P
   begin
     ...
   end P;
 
--------------------------
Library Unit Subprograms
--------------------------

* Recall separate declarations are optional

   - Body can act as declaration if no declaration provided

* Separate declaration provides usual benefits

   - Changes/recompilation to body only require relinking clients

* File 1 (p.ads for GNAT)

   .. code:: Ada

      procedure P (F : in Integer);
 
* File 2 (p.adb for GNAT)

   .. code:: Ada

      procedure P (F : in Integer) is
      begin
        ...
      end P;
 
--------------------------
Library Unit Subprograms
--------------------------

* Specifications in declaration and body must conform

   - Example

      + Spec for P

      .. code:: Ada

         procedure P (F : in integer);
 
      + Body for P

      .. code:: Ada

         procedure P (F : in float) is
         begin
         ...
         end P;
 
   - Declaration creates subprogram `P` in library
   - Declaration exists so body does not act as declaration
   - Compilation of file "p.adb" must fail

* New declaration with same name replaces old one
* Thus cannot overload library units

------------------
Main Subprograms
------------------

.. container:: columns

 .. container:: column
  
    * Must be library subprograms
    * No special program unit name required
    * Can be many per program library

    * Always can be procedures 

    * Can be functions if implementation allows it

       - Execution environment must know how to handle result

 .. container:: column
  
    .. code:: Ada
    
       with Ada.Text_IO;
       procedure Hello is
       begin
         Ada.Text_IO.Put(
             "Hello World" );
       end Hello;
     
================
"with" Clauses
================

----------
Examples
----------

.. include:: examples/130_program_structure/with_clauses.rst

-----------------
 `with` Clauses
-----------------

* Specify the library units that a compilation unit depends upon

   - The "context" in which the unit is compiled

* Syntax (simplified)

   .. code:: Ada

      context_clause ::= { context_item }
      context_item ::= with_clause | use_clause
      with_clause ::= with library_unit_name
                      { , library_unit_name };
 
.. code:: Ada

   with Ada.Text_IO; -- dependency
   procedure Hello is
   begin
     Ada.Text_IO.Put ("Hello World");
   end Hello;
 
-----------------------
`with` Clauses Syntax
-----------------------

* Helps explain restrictions on library units

   - No overloaded library units
   - If overloading allowed, which `P` would `with P;` refer to?
   - No library unit functions names as operators

      + Mostly because of no overloading

----------------
What To Import
----------------

* Need only name direct dependencies

   - Those actually referenced in the corresponding unit

* Will not cause compilation of referenced units

   - Unlike "include directives" of some languages

.. code:: Ada
    
   package A is
     type Something is ...
   end A;
       
   with A;
   package B is
     type Something is record
       Field : A.Something;
     end record;
   end B;
       
   with B; -- no "with" of A 
   procedure Foo is
     X : B.Something;
   begin
     X.Field := ...
 
--------------------------------
Imported Interface Consistency
--------------------------------

* Use consistent with imported interface is guaranteed by compiler

   - Compiler rejects incompatible use

      + Wrong subprogram name
      + Wrong parameters
      + Et cetera

   - Linker/binder rejects inclusion of obsolete units

* Thus cannot create executable with invalid interfaces

   - Even in the distributed programming case!

* A separate tool in other languages

   - At additional cost

-------------------------
`with` Clause Placement
-------------------------

* May have several clauses per library unit

   .. code:: Ada

      with A;
      with B;
      package C is ...
 
* Not allowed in inner scopes

   - (Other uses of reserved word `with` do occur inside)

      .. code:: Ada

        package C is ...
           with B; -- illegal
 
--------------------------
`with` Clause Visibility
--------------------------

.. code:: Ada

   with Ada.Text_IO;
   with Real_IO, Int_IO;
   with Math_Library;
   package P is
   ...
   end P;
   
   package body P is
     -- no need to add "with Math_Library;" to body
     Num : Math_Library.Real_Number; 
     ...
   end P;
 
-----------------------------------------
Put Dependencies In Body When Possible 
-----------------------------------------

* Change to given unit requires client recompilation
* Recompilation of client declaration can trigger transitive recompilation in all its clients

   - In some implementations

* If `Math_Library` changes, `P` and anything that depends on `P` might have to recompile

   .. code:: Ada

      with Math_Library;
      package P is
        Num : Math_Library.Real_Number;
      end P;
 
* If `Real_IO` changes, only the body of `P` should need to recompile

   .. code:: Ada

      with Real_IO;
      package body P is
      ...
      end P;
 
=========================
"limited with" Clauses
=========================

----------
Examples
----------

.. include:: examples/130_program_structure/limited_with_clauses.rst

------------------------------
Handling Cyclic Dependencies
------------------------------

.. container:: columns

 .. container:: column
  
    * Elaboration must be linear
    * Package declarations cannot depend on each other

       - No linear order is possible

    * Which package elaborates first?

 .. container:: column
  
    .. image:: ../../images/cyclic_dependencies.png

--------------------------------------
Body-Level Cross Dependencies Are OK
--------------------------------------

.. container:: columns

 .. container:: column
  
    * The bodies only depend on other packages' declarations
    * The declarations are already elaborated by the time the bodies are elaborated

 .. container:: column
  
    .. image:: ../../images/mutual_dependencies.png
    
--------------------------
Resulting Design Problem
--------------------------

* Good design dictates that conceptually distinct types appear in distinct package declarations

   - Separation of concerns
   - High level of "cohesion"

* Not possible if they depend on each other
* One solution is to combine them in one package, even though conceptually distinct

   - Poor software engineering

----------------------------------------
Illegal Package Declaration Dependency
----------------------------------------

.. code:: Ada
    
   with Department;
   package Personnel is
     type Employee is private;
     procedure Assign ( This : in Employee;
                        To : in out Department.Section);
   private
     type Employee is record
       Assigned_To : Department.Section;
     end record;
   end Personnel;
     
   with Personnel;
   package Department is
     type Section is private;
     procedure Choose_Manager ( This : in out Section;
                                Who : in Personnel.Employee);
   private
     type Section is record
       Manager : Personnel.Employee;
     end record;
   end Department;
     
------------------------
`limited with` Clauses
------------------------

.. admonition:: Language Variant

   Ada 2005

.. container:: columns

 .. container:: column
  
    * Solve the cyclic declaration dependency problem

       - Controlled cycles are now permitted

    * Provide a "limited" view of the specified package

       - Only type names are visible (including in nested packages)
       - Types are viewed as "incomplete types"

 .. container:: column
  
    * Normal view
    
       .. code:: Ada
    
          package Personnel is
            type Employee is private;
            procedure Assign ...
          private
            type Employee is ...
          end Personnel;
     
    * Implied limited view
    
       .. code:: Ada
    
          package Personnel is
            type Employee;
          end Personnel;
     
.. container:: speakernote

   Note that the names of nested packages are of course visible, otherwise we could not reference the names of types declared within them.

------------------------
Using Incomplete Types
------------------------

* Anywhere that the compiler doesn't yet need to know how they are really represented

   - Access types designating them
   - Access parameters designating them
   - Anonymous access components designating them
   - As formal parameters and function results

      + As long as compiler knows them at the point of the call

   - As generic formal type parameters
   - As introductions of private types

* If `tagged`, may also use `'Class`
* Thus typically involves some advanced features

--------------------------------------
Legal Package Declaration Dependency
--------------------------------------

.. admonition:: Language Variant

   Ada 2005

.. code:: Ada
    
   limited with Department;
   package Personnel is
     type Employee is private;
     procedure Assign ( This : in Employee;
                        To : in out Department.Section);
   private
     type Employee is record
       Assigned_To : access Department.Section;
     end record;
   end Personnel;
     
   limited with Personnel;
   package Department is
     type Section is private;
     procedure Choose_Manager ( This : in out Section;
                                Who : in Personnel.Employee);
   private
     type Section is record
       Manager : access Personnel.Employee;
     end record;
   end Department;
     
----------------------------------------
Full `with` Clause On the Package Body
----------------------------------------

.. admonition:: Language Variant

   Ada 2005

* Even though declaration has a `limited with` clause
* Typically necessary since body does the work

   - Dereferencing, etc.

* Usual semantics from then on

   .. code:: Ada

      limited with Personnel;
      package Department is
      ...
      end Department;
      
      with Personnel; -- normal view in body
      package body Department is
      ...
      end Department;
 
============================
Hierarchical Library Units
============================

----------
Examples
----------

.. include:: examples/130_program_structure/hierarchical_library_units.rst

----------------------------------
Problem: Packages Are Not Enough
----------------------------------

* Extensibility is a problem for private types

   - Provide excellent encapsulation and abstraction
   - But one has either complete visibility or essentially none
   - New functionality must be added to same package for sake of compile-time visibility to representation
   - Thus enhancements require editing/recompilation/retesting

* Should be something "bigger" than packages

   - Subsystems
   - Directly relating library items in one name-space

      + One big package has too many disadvantages

   - Avoiding name clashes among independently-developed code

--------------------------------------
Solution: Hierarchical Library Units
--------------------------------------

.. container:: columns

 .. container:: column
  
    * Address extensibility issue

       - Can extend packages with visibility to parent private part
       - Extensions do not require recompilation of parent unit
       - Visibility of parent's private part is protected

    * Directly support subsystems

       - Extensions all have the same ancestor "root" name

 .. container:: column
  
    .. image:: ../../images/hierarchical_library_units.png
    
--------------------------
Programming By Extension
--------------------------

* Parent unit
    
   .. code:: Ada
    
      package Complex is
        type Number is private;
        function "*" ( Left, Right : Number ) return Number;
        function "/" ( Left, Right : Number ) return Number;
        function "+" ( Left, Right : Number ) return Number;
        function "-" ( Left, Right : Number ) return Number;
      ...
      private
        type Number is record
          Real_Part, Imaginary_Part : Float;
        end record;
      end Complex;
     
* Extension created to work with parent unit
    
   .. code:: Ada
    
      package Complex.Utils is
        procedure Put (C : in Number);
        function As_String (C : Number) return String;
        ...
      end Complex.Utils;
     
----------------------------------------
Extensions Can See Type Representation
----------------------------------------

.. code:: Ada

   with Ada.Text_IO;
   package body Complex.Utils is
     procedure Put( C : in Number ) is
     begin
       Ada.Text_IO.Put( As_String(C) );
     end Put;
     function As_String( C : Number ) return String is
     begin
       -- Real_Part and Imaginary_Part are
       -- visible to child's body
       return "( " & Float'Image(C.Real_Part) & ", " &
              Float'Image(C.Imaginary_Part) & " )";
     end As_String;
   ...
   end Complex.Utils;
 
--------------------
Subsystem Approach
--------------------

.. code:: Ada
    
   with Interfaces.C;  
   package OS is -- Unix and/or POSIX
    type File_Descriptor is new Interfaces.C.int;
     ... 
   end OS;
     
   package OS.Mem_Mgmt is
     ...
     function Mmap ( Requested_Location : System.Address; 
                     Requested_Size  : Interfaces.C.size_t;
                     Offset_Within_Area : Interfaces.C.int )
                     return System.Address;
     ...
   end OS.Mem_Mgmt;
       
   package OS.Files is
     ...
     function Open ( Device : Interfaces.C.char_array; 
                     Permission : Permissions := S_IRWXO )
                     return File_Descriptor;
     ...
   end OS.Files;
     
------------------------
Predefined Hierarchies
------------------------

* Standard library facilities are children of `Ada`

   - `Ada.Text_IO`
   - `Ada.Calendar`
   - `Ada.Command_Line`
   - `Ada.Exceptions`
   - et cetera

* Other root packages are also predefined

   - `Interfaces.C`
   - `Interfaces.Fortran`
   - `Interfaces.COBOL`
   - `System.Storage_Pools`
   - `System.Storage_Elements`
   - et cetera

-------------------------
Hierarchical Visibility
-------------------------

.. container:: columns

 .. container:: column
  
    * Children can see ancestors' visible and private parts

       - All the way up to the root library unit

    * Siblings have no automatic visibility to each other
    * Visibility as If Nested

       - As if child library units are nested within parents

          + All child units come after the root parent's specification
          + Grandchildren within children, great-grandchildren within ...

 .. container:: column
  
    .. image:: ../../images/hierarchical_visibility.png
    
------------------------------------
Example of Visibility As If Nested
------------------------------------

.. code:: Ada

   package Complex is
     type Number is private;
     function "*" (Left, Right : Number) return Number;
     function "/" (Left, Right : Number) return Number;
     function "+" (Left, Right : Number) return Number;
     ...
   private
     type Number is record
       Real_Part : Float;
       Imaginary : Float;
     end record;
     package Utils is
       procedure Put (C : in Number);
       function As_String (C : Number) return String;
       ...
     end Utils;
   end Complex;
 
-------------------------------------------
`with` Clauses for Ancestors are Implicit
-------------------------------------------

.. container:: columns

 .. container:: column
  
    * Because children can reference ancestors' private parts

       - Code is not in executable unless somewhere in the `with` clauses

    * Explicit clauses for ancestors are redundant but OK 

 .. container:: column
  
    .. code:: Ada
    
       package Parent is
         ...
       private
         A : Integer := 10;
       end Parent;
       
       -- no "with" of parent needed
       package Parent.Child is
          ...
       private
         B : Integer := Parent.A;
         -- no dot-notation needed
         C : integer := A;
       end Parent.Child;
     
-------------------------------------------
 `with` Clauses for Siblings are Required
-------------------------------------------

* If references are intended

.. code:: Ada

   with A.Foo; --required
   package body A.Bar is
      ...
      -- 'Foo' is directly visible because of the
      -- implied nesting rule
      X : Foo.Typemark; 
   end A.Bar;
 
===================
Visibility Limits
===================

----------
Examples
----------

.. include:: examples/130_program_structure/visibility_limits.rst

----------------------------
The "Howard Hughes" Effect
----------------------------

* Children grant themselves access to ancestors' private parts

   - May be created well after parent
   - Parent doesn't know if/when child packages will exist
   - Thus the "Howard Hughes" Effect

* Alternative is to grant access when declared

   - Like ``friend`` units in C++
   - But would have to be prescient!
   - Hence too restrictive

----------------------------------------------
Correlation to C++ Class Visibility Controls
----------------------------------------------

.. container:: columns

 .. container:: column

   * Ada private part is visible to child units

      .. code:: Ada

         package P is
           A ...
         private
           B ...
         end P;
         package body P is
           C ...
         end P;
 
 .. container:: column

   * Thus private part is like the protected part in C++

      .. code:: C++

         class C {
         public:
           A ...
         protected:
           B ...
         private:
           C ...
         };
 
-------------------
Visibility Limits
-------------------

* Visibility to parent's private part is not open-ended

   - Only visible to private parts and bodies of children
   - As if only private part of child package is nested in parent

* Recall users can only reference exported declarations

   - Child public spec only has access to parent public spec

.. code:: Ada
    
   package Parent is
      ...
   private
      type Parent_T is ...
   end Parent;
       
   package Parent.Child is
     -- Parent_T is not visible here!
   private
     -- Parent_T is visible here
   end Parent.Child;
       
   package body Parent.Child is
    -- Parent_T is visible here
   end Parent.Child;
     
--------------------------
Misbehaving (?) Children
--------------------------

* Can break a parent's abstraction

  - Exporting a subprogram that alters package state
  - Exporting a subprogram that alters ADT object state

* Nice for testing via fault injection...

.. code:: Ada

   package Stack is
     procedure Push ( X : in Foo );
     procedure Pop ( X : out Foo );
   private
     Values : array (1 .. N ) of Foo;
     Top : Natural range 0 .. N := 0
   end Stack;
 
   package Stack.Child is
     procedure Misbehave;
     procedure Reset;
   end Stack.Child;

   package body Stack.Child is
     procedure Misbehave is
     begin
       Top := 0;
     end Misbehave;
 
     procedure Reset is
     begin
       Top := 0;
     end Reset;
   end Stack.Tools;

---------------------------
Another Misbehaving Child
---------------------------

* Can indirectly export parent's private information

.. code:: Ada

   package Skippy is
      ...
   private
     X : Integer := 0;
   end Skippy;
 
.. code:: Ada

   package Skippy.Evil_Twin is
     function Cheater return Integer;
   end Skippy.Evil_Twin;
   package body Skippy.Evil_Twin is
     function Cheater return Integer is
     begin
       return X;
     end Cheater;
   end Skippy.Evil_Twin;
 
===================
Private Children
===================

----------
Examples
----------

.. include:: examples/130_program_structure/private_children.rst

------------------
Private Children
------------------

* Intended as implementation artifacts
* Only available within subsystem

   - Rules prevent `with` clauses by clients 

   - Thus cannot export anything outside subsystem
   - Thus have no parent visibility restrictions

      + Public part of child also has visibility to ancestors' private parts

.. code:: Ada

  private package Maze.Debug is
     procedure Dump_State;
     ...
  end Maze.Debug;

-------------------------------------------
Rules Preventing Private Child Visibility
-------------------------------------------

* Only available within immediate family

   - Rest of subsystem cannot import them

* Public unit declarations have import restrictions

   - To prevent re-exporting private information

* Public unit bodies have no import restrictions

   - Since can't re-export any imported info

* Private units can import anything

   - Declarations and bodies can import public and private units
   - Cannot be imported outside subsystem so no restrictions

--------------
Import Rules
--------------

* Only parent of private unit and its descendants can import a private child
* Public unit declarations import restrictions

   - Not allowed to have `with` clauses for private units

      + Exception explained in a moment

   - Precludes re-exporting private information

* Private units can import anything

   - Declarations and bodies can import private children

--------------------------------------
Some Public Children Are Trustworthy
--------------------------------------

* Would only use a private sibling's exports privately
* But rules disallow `with` clause
    
.. code:: Ada
    
   private package OS.UART is
    type Device is limited private;
    procedure Open (This : out Device; ...);
    ...
   end OS.UART;
     
   -- illegal - private child
   with OS.UART;
   package OS.Serial is
     type COM_Port is limited private;
     ...
   private
     type COM_Port is limited record
       -- but I only need it here!
       COM : OS.UART.Device;
     ...
     end record;
   end OS.Serial;
     
-----------------------------------------
Solution 1: Move Type To Parent Package
-----------------------------------------

.. code:: Ada
    
   package OS is
     ...
   private
     -- no longer an ADT!
     type Device is limited private;
   ...
   end OS;
   private package OS.UART is
     procedure Open (This : out Device;
      ...);
     ...
   end OS.UART;
     
.. code:: Ada
    
   package OS.Serial is
     type COM_Port is limited private;
     ...
   private
     type COM_Port is limited record
       COM : Device; -- now visible
       ...
     end record;
   end OS.Serial;

-------------------------------------------
Solution 2: Partially Import Private Unit
-------------------------------------------

.. admonition:: Language Variant

   Ada 2005

* Via `private with` clause
* Syntax

   .. code:: Ada

      private with package_name {, package_name} ;
 
* Public declarations can then access private siblings

   - But only in their private part
   - Still prevents exporting contents of private unit

* The specified package need not be a private unit

   - But why bother otherwise

------------------------
`private with` Example
------------------------

.. admonition:: Language Variant

   Ada 2005

.. code:: Ada
    
   private package OS.UART is
     type Device is limited private;
     procedure Open (This : out Device; 
        ...);
     ...
   end OS.UART;
     
.. code:: Ada
    
   private with OS.UART;
   package OS.Serial is
     type COM_Port is limited private;
     ...
   private
     type COM_Port is limited record
       COM : OS.UART.Device;
       ...
     end record;
   end OS.Serial;

-------------------------------------
Combining Private and Limited Withs
-------------------------------------

.. admonition:: Language Variant

   Ada 2005

* Cyclic declaration dependencies allowed
* A public unit can `with` a private unit
* With-ed unit only visible in the private part

.. code:: Ada

   limited with Parent.Public_Child;
   private package Parent.Private_Child is
     type T is ... 
   end Parent.Private_Child;
   
   limited private with Parent.Private_Child;
   package Parent.Public_Child is
     ...
   private
     X : access Parent.Private_Child.T;
   end Parent.Public_Child;
 
--------------------------------
Completely Hidden Declarations
--------------------------------

* Anything in a package body is completely hidden

   - Children have no access to package bodies

* Precludes extension using the entity

   - Must know that children will never need it

.. code:: Ada

   package body Skippy is
     X : Integer := 0;
     ...
   end Skippy;
 
-------------------
Child Subprograms
-------------------

* Child units can be subprograms

   - Recall syntax
   - Both public and private child subprograms

* Separate declaration required if private

   - Syntax doesn't allow `private` on subprogram bodies

* Only library packages can be parents

   - Only they have necessary scoping

.. code:: Ada

   private procedure Parent.Child;
 
------------------------------------
Hierarchical Library Units Summary
------------------------------------

* Parents should document assumptions for children

   - "These must always be in ascending order!"

* Children cannot misbehave unless imported ("with'ed")

* The writer of a child unit must be trusted 

   - As much as if he or she were to modify the parent itself

===============
"use" Clauses
===============

----------
Examples
----------

.. include:: examples/130_program_structure/use_clauses.rst

----------------
 `use` Clauses
----------------

* Provide direct visibility into packages' exported items
* May still use expanded name

.. code:: Ada
    
   package Ada.Text_IO is
     procedure Put_Line( ... );
     procedure New_Line( ... );
     ...
   end Ada.Text_IO;
       
   with Ada.Text_IO;
   procedure Hello is
     use Ada.Text_IO;
   begin
     Put_Line( "Hello World" );
     New_Line(3);
     Ada.Text_IO.Put_Line ( "Good bye" );
   end Hello;
     
---------------------
`use` Clause Syntax
---------------------

* May have several, like `with` clauses
* Must name an imported library unit

   - From same context clause 

* Syntax

   .. code:: Ada

      context_clause ::= {context_item}
      context_item ::= with_clause | use_clause
      use_clause ::= use_package_clause | use_type_clause
      use_package_clause ::= use package_name {, package_name};
      use_type_clause ::= use [all] type subtype_mark
                                         {, subtype_mark};
 
* Can only `use` a library package

   - Subprograms don't make sense

--------------------
`use` Clause Scope
--------------------

* Applies to end of body, from first occurrence

.. code:: Ada

   with Pkg_A;
   with Pkg_B;
   use Pkg_A;
   package P is
     -- all of Pkg_A is visible starting here
     -- references to Pkg_B must use dot-notation
     X : integer :=
     use Pkg_B;
     -- all of Pkg_B is now visible
     ...
   end P;
   
   package body P is
     -- all of Pkg_A and Pkg_B is visible here
   end P;
 
--------------------
No Meaning Changes
--------------------

* A new `use` clause won't change a program's meaning!
* Any directly visible names still refer to the original entities

.. code:: Ada
    
   package D is
     T : Real;
   end D;
       
   with D;
   procedure P is
     procedure Q is
       T, X : Real;
     begin
       ...
       declare
         use D;
       begin
         -- With or without the clause, "T" means Q.T
         X := T;
       end;
       ...
     end Q;
     
---------------------------
No Ambiguity Introduction
---------------------------

.. code:: Ada

   package D is
     V : Boolean;
   end D;
   
   package E is
     V : Integer;
   end E;
   with D, E;
   
   procedure P is
     procedure Q is
       use D, E;
     begin
       -- to use V here, must specify D.V or E.V
       ...
     end Q;
   begin
   ...
 
.. container:: speakernote

   For declarations in different packages that would not be directly visible in the absence of a "use" clause, none with the same identifier will be directly visible in the presence of such a clause, unless both are overloadable (i.e., enumeration literals and subprogram declarations)

------------------------------
`use` Clauses and Child Units
------------------------------

* A clause for a child does not imply one for its parent
* A clause for a parent makes the child directly visible

   - Since children are 'inside' declarative region of parent

.. code:: Ada
    
   package Parent is
     P1 : Integer;
     ...
   end Parent;
       
   package Parent.Child is
     PC1 : Integer;
     ...
   end Parent.Child;
       
   with Parent.Child;
   procedure Demo is
     D1 : Integer := Parent.P1;
     D2 : Integer := Parent.Child.PC1;
     use Parent;
     D3 : Integer := P1;
     D4 : Integer := Child.PC1; 
   begin
     ...
   end Demo;
     
.. container:: speakernote

   D4 has access to CHILD because PARENT is "use"d

----------------------------------------
`use` Clause and Implicit Declarations
----------------------------------------

* Visibility rules apply to implicit declarations too

.. code:: Ada

   package P is
     type Int is range Lower .. Upper;
     -- implicit declarations
     -- function "+"( Left, Right : Int ) return Int;
     -- function "="( Left, Right : Int ) return Boolean;
   end P;
   
   with P;
   procedure Test is
     A, B, C : P.Int := some_value;
   begin
     C := A + B; -- illegal reference to operator
     C:= P."+" (A,B);
     declare
       use P;
     begin
       C := A + B; -- now legal
     end;
   end Test;
 
====================
"use type" Clauses
====================

----------
Examples
----------

.. include:: examples/130_program_structure/use_type_clauses.rst

---------------------
`use type` Clauses
---------------------

* Syntax

   .. code:: Ada

      context_clause ::= {context_item}
      context_item ::= with_clause | use_clause
      use_clause ::= use_package_clause | use_type_clause
      use_package_clause ::= use package_name {, package_name};
      use_type_clause ::= use [all] type subtype_mark
                                         {, subtype_mark};
 
* Makes operators directly visible for specified type

   - Implicit and explicit operator function declarations
   - Only those that mention the type in the profile

      + Parameters and/or result type

* More specific alternative to `use` clauses

   - Especially useful when multiple `use` clauses introduce ambiguity

---------------------------
`use type` Clause Example
---------------------------

.. code:: Ada

   package P is
     type Int is range Lower .. Upper;
     -- implicit declarations
     -- function "+"( Left, Right : Int ) return Int;
     -- function "="( Left, Right : Int ) return Boolean;
   end P;
   with P;
   procedure Test is
     A, B, C : P.Int := some_value;
     use type P.Int;
     D : Int; -- not legal
   begin
     C := A + B; -- operator is visible
   end Test;
 
--------------------------------------
`use Type` Clauses and Multiple Types
--------------------------------------

* One clause can make ops for several types visible

   - When multiple types are in the profiles

* No need for multiple clauses in that case

.. code:: Ada

   package P is
     type T1 is range 1 .. 10;
     type T2 is range 1 .. 10;
     type T3 is range 1 .. 10;
     -- "use type" on any of T1, T2, T3
     -- makes operator visible
     function "+"( Left : T1; Right : T2 ) return T3;
   end P;

-----------------------------
Multiple `use type` Clauses
-----------------------------

* May be necessary
* Only those that mention the type in their profile are made visible

.. code:: Ada

   package P is
     type T1 is range 1 .. 10;
     type T2 is range 1 .. 10;
     -- implicit
     -- function "+"( Left : T2; Right : T2 ) return T2;
     type T3 is range 1 .. 10;
     -- explicit
     function "+"( Left : T1; Right : T2 ) return T3;
   end P;
   
   with P;
   procedure UseType is
     X1 : P.T1;
     X2 : P.T2;
     X3 : P.T3;
     use type P.T1;
   begin
     X3 := X1 + X2; -- operator visible because it uses T1
     X2 := X2 + X2; -- operator not visible
   end UseType;
 
========================
"use all type" Clauses
========================

----------
Examples
----------

.. include:: examples/130_program_structure/use_all_type_clauses.rst

-------------------------
`use all type` Clauses
-------------------------

.. admonition:: Language Variant

   Ada 2012

* Makes all primitive operations for the type visible

   - Not just operators
   - Especially, subprograms that are not operators

* **Primitive operation** has a precise meaning

   - Predefined operations such as ``=`` and ``+``  etc.
   - Subprograms declared in the same package as the type and which operate on the type
   - Inherited or overridden subprograms
   - For `tagged` types, class-wide subprograms
   - Enumeration literals

* Still need a `use` clause for other entities

   - Typically exceptions

-------------------------------
`use all type` Clause Example
-------------------------------

.. admonition:: Language Variant

   Ada 2012

.. code:: Ada
    
   package Complex is
     type Number is private;
     function "*" (Left, Right : Number) return Number;
     function "/" (Left, Right : Number) return Number;
     function "+" (Left, Right : Number) return Number;
     procedure Put (C : Number);
     procedure Make ( C : out Number;
                      From_Real, From_Imag : Float );
     procedure Non_Primitive ( X : Integer );
       ... 
     
   with Complex;
   use all type Complex.Number;
   procedure Demo is 
     A, B, C : Complex.Number;
   begin
     -- "use all type" makes these available
     Make (A, From_Real => 1.0, From_Imag => 0.0);
     Make (B, From_Real => 1.0, From_Imag => 0.0);
     C := A + B;
     Put (C);
     -- but not this one
     Non_Primitive (0);
   end Demo;
     
--------------------------------------
`use all type` v. `use type` Example
--------------------------------------

.. admonition:: Language Variant

   Ada 2012

.. code:: Ada

   with Complex;   use type Complex.Number;
   procedure Demo is 
     A, B, C : Complex.Number;
   Begin
     -- these are always allowed
     Complex.Make (A, From_Real => 1.0, From_Imag => 0.0);
     Complex.Make (B, From_Real => 1.0, From_Imag => 0.0);
     -- "use type" does not give access to these
     Make (A, 1.0, 0.0); -- not visible
     Make (B, 1.0, 0.0); -- not visible
     -- but this is good
     C := A + B;
     Complex.Put (C);
     -- this is not allowed
     Put (C); -- not visible
   end Demo;

===================
Renaming Entities
===================

---------------------------------
Three Positives Make a Negative
---------------------------------

* Good Coding Practices ...

   - Descriptive names
   - Modularization
   - Subsystem hierarchies

* Can result in cumbersome references

   .. code:: Ada

      -- use cosine rule to determine distance between two points,
      -- given angle and distances between observer and 2 points
      -- A**2 = B**2 + C**2 - 2*B*C*cos(A)
      Observation.Sides (Viewpoint_Types.Point1_Point2) :=
        Math_Utilities.Trigonometry.Square_Root
          (Observation.Sides (Viewpoint_Types.Observer_Point1)**2 +
           Observation.Sides (Viewpoint_Types.Observer_Point2)**2 +
           2.0 * Observation.Sides (Viewpoint_Types.Observer_Point1) *
             Observation.Sides (Viewpoint_Types.Observer_Point2) *
             Math_Utilities.Trigonometry.Cosine
               (Observation.Vertices (Viewpoint_Types.Observer)));

--------------------------------
Writing Readable Code - Part 1
--------------------------------

* We could use `use` on package names to remove some dot-notation

   .. code:: Ada

      -- use cosine rule to determine distance between two points, given angle
      -- and distances between observer and 2 points A**2 = B**2 + C**2 -
      -- 2*B*C*cos(A)
      Observation.Sides (Point1_Point2) :=
        Square_Root
          (Observation.Sides (Observer_Point1)**2 +
           Observation.Sides (Observer_Point2)**2 +
           2.0 * Observation.Sides (Observer_Point1) *
             Observation.Sides (Observer_Point2) *
             Cosine (Observation.Vertices (Observer)));

* But that only shortens the problem, not simplifies it

   - If there are multiple "use" clauses in scope:

      + Reviewer may have hard time finding the correct definition
      + Homographs may cause ambiguous reference errors

* We want the ability to refer to certain entities by another name (like an alias) with full read/write access (unlike temporary variables)
      
-----------------------
The `renames` Keyword
-----------------------

* Certain entities can be renamed within a declarative region

   - Packages

      .. code:: Ada

         package Math renames Math_Utilities.Trigonometry

   - Objects (or elements of objects)

      .. code:: Ada

         Angles : Viewpoint_Types.Vertices_Array_T
                  renames Observation.Vertices;
         Required_Angle : Viewpoint_Types.Vertices_T
                  renames Viewpoint_Types.Observer;

   - Subprograms

      .. code:: Ada

         function Sqrt (X : Base_Types.Float_T)
                        return Base_Types.Float_T
                        renames Math.Square_Root;

--------------------------------
Writing Readable Code - Part 2
--------------------------------

* With `renames` our complicated code example is easier to understand

   .. code:: Ada

      begin
         Side1 : Base_Types.Float_T renames
           Observation.Sides (Viewpoint_Types.Observer_Point1);
         Side2 : Base_Types.Float_T renames
           Observation.Sides (Viewpoint_Types.Observer_Point2);
         Angles : Viewpoint_Types.Vertices_Array_T renames Observation.Vertices;
         Required_Angle : Viewpoint_Types.Vertices_T renames
           Viewpoint_Types.Observer;
         Desired_Side : Base_Types.Float_T renames
           Observation.Sides (Viewpoint_Types.Point1_Point2);

         package Math renames Math_Utilities.Trigonometry;

         function Sqrt (X : Base_Types.Float_T) return Base_Types.Float_T
           renames Math.Square_Root;

      begin

         Side1                   := Sensors.Read;
         Side2                   := Sensors.Read;
         Angles (Required_Angle) := Sensors.Read;

         -- use cosine rule to determine distance between two points, given angle
         -- and distances between observer and 2 points A**2 = B**2 + C**2 -
         -- 2*B*C*cos(A)
         Desired_Side := Sqrt (Side1**2 + Side2**2 +
                               2.0 * Side1 * Side2 * Math.Cosine (Angles (Required_Angle)));

      end;

========
Lab
========

.. include:: labs/130_program_structure.lab.rst

=========
Summary
=========

---------
Summary
---------

.. admonition:: Language Variant

   Ada 2012

* `with` clauses interconnect library units

   - Express dependencies of the one being compiled
   - Not textual inclusion!

* Hierarchical library units address important issues

   - Direct support for subsystems
   - Extension without recompilation
   - Separation of concerns with controlled sharing of visibility

* `use` clauses are not evil but can be abused
* `use all type` clauses are more likely in practice than `use type` clauses

   - Only available in Ada 2012 and later

* `Renames` allow us to alias entities to make code easier to read

   - Subprogram renaming has many other uses, such as adding / removing default parameter values
