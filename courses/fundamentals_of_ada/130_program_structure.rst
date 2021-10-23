
*******************
Program Structure
*******************

.. role:: ada(code)
    :language: Ada

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

===================
Building A System
===================

-------------------
What is a System?
-------------------

* Also called Application or Program or ...
* Collection of library units

   - Which are a collection of packages, subprograms, objects

----------------------
Library Units Review
----------------------

* Those units not nested within another program unit
* Candidates

   - Subprograms
   - Packages
   - Generic Units
   - Generic Instantiations
   - Renamings

* Dependencies between library units via :ada:`with` clauses
* What about extending existing packages?

   - How to break privacy for some packages?

============================
Hierarchical Library Units
============================

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

       - Extensions all have the same ancestor *root* name

 .. container:: column

    .. image:: hierarchical_library_units.png

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

-----------------------------------
Extension Can See Private Section
-----------------------------------

* With certain limitations

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
     procedure Dump ( File               : File_Descriptor;
                      Requested_Location : System.Address;
                      Requested_Size     : Interfaces.C.Size_T );
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
    * Visibility same as nested

       - As if child library units are nested within parents

          + All child units come after the root parent's specification
          + Grandchildren within children, great-grandchildren within ...

 .. container:: column

    .. image:: hierarchical_visibility.png

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

       - Code is not in executable unless somewhere in the :ada:`with` clauses

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

------
Quiz
------

.. code:: Ada

   package Parent is
      Parent_Object : Integer;
   end Parent;

   package Parent.Sibling is
      Sibling_Object : Integer;
   end Parent.Sibling;

   package Parent.Child is
      Child_Object : Integer := ? ;
   end Parent.Child;

Which is not a legal initialization of Child_Object?

   A. ``Parent.Parent_Object + Parent.Sibling.Sibling_Object``
   B. ``Parent_Object + Sibling.Sibling_Object``
   C. ``Parent_Object + Sibling_Object``
   D. :answer:`All of the above`

.. container:: animate

   A, B, and C are illegal because there is no reference to package
   :ada:`Parent.Sibling` (the reference to :ada:`Parent` is implied by the
   hierarchy). If :ada:`Parent.Child` had ":ada:`with Parent.Sibling;`", then
   A and B would be legal, but C would still be incorrect because there is
   no implied reference to a sibling.

===================
Visibility Limits
===================

-------------------------------------
Parents Do Not Know Their Children!
-------------------------------------

* Children grant themselves access to ancestors' private parts

   - May be created well after parent
   - Parent doesn't know if/when child packages will exist

* Alternative is to grant access when declared

   - Like ``friend`` units in C++
   - But would have to be prescient!

      * Or else adding children requires modifying parent

   - Hence too restrictive

* Note: Parent body can reference children

   - Typical method of parsing out complex processes

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

--------------------------------
Children Can Break Abstraction
--------------------------------

* Could **break** a parent's abstraction

  - Alter a parent package state
  - Alters an ADT object state

* Useful for reset, testing: fault injections...

.. code:: Ada

   package Stack is
      ...
   private
      Values : array (1 .. N ) of Foo;
      Top : Natural range 0 .. N := 0
   end Stack;

   package body Stack.Reset is
      procedure Reset is
      begin
        Top := 0;
      end Reset;
   end Stack.Tools;

--------------------------
Using Children for Debug
--------------------------

* Provide **accessors** to parent's private information
* eg internal metrics...

.. code:: Ada

   package P is
      ...
   private
     Internal_Counter : Integer := 0;
   end P;

.. code:: Ada

   package P.Child is
     function Count return Integer;
   end P.Child;

.. code:: Ada

   package body P.Child is
     function Count return Integer is
     begin
       return Internal_Counter;
     end Count;
   end P.Child;

------
Quiz
------

.. container:: latex_environment scriptsize

 .. container:: columns

  .. container:: column

   .. code:: Ada

      package P is
         procedure Initialize;
         Object_A : Integer;
      private
         Object_B : Integer;
      end P;

      package body P is
         Object_C : Integer;
         procedure Initialize is null;
      end P;

      package P.Child is
         function X return Integer;
      end P.Child;

  .. container:: column

   Which return statement would be illegal in P.Child.X?

      A.  ``return Object_A;``
      B.  ``return Object_B;``
      C.  ``return Object_C;``
      D.  None of the above

   .. container:: animate

      Explanations

      A. :ada:`Object_A` is in the public part of :ada:`P` - visible to any unit that :ada:`with`'s :ada:`P`
      B. :ada:`Object_B` is in the private part of :ada:`P` - visible in the private part or body of any descendant of :ada:`P`
      C. :ada:`Object_C` is in the body of :ada:`P`, so it is only visible in the body of :ada:`P`
      D. A and B are both valid completions

=========
Summary
=========

---------
Summary
---------

* Hierarchical library units address important issues

   - Direct support for subsystems
   - Extension without recompilation
   - Separation of concerns with controlled sharing of visibility

* Parents should document assumptions for children

   - "These must always be in ascending order!"

* Children cannot misbehave unless imported ("with'ed")
* The writer of a child unit must be trusted

   - As much as if he or she were to modify the parent itself
