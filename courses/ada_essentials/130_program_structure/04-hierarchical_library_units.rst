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

    * Address extensibility issue

       - Can extend packages with visibility to parent private part
       - Extensions do not require recompilation of parent unit
       - Visibility of parent's private part is protected

    * Directly support subsystems

       - Extensions all have the same ancestor *root* name

-------------------------------
Visibility Across a Hierarchy
-------------------------------

.. container:: overlay 1

   .. image:: hierarchical_visibility_1.svg
      :width: 70%
      :align: center

.. container:: overlay 2

   .. image:: hierarchical_visibility_2.svg
      :width: 70%
      :align: center

.. container:: overlay 3

   .. image:: hierarchical_visibility_3.svg
      :width: 70%
      :align: center

.. container:: overlay 4

   .. image:: hierarchical_visibility_4.svg
      :width: 70%
      :align: center

--------------------------
Programming by Extension
--------------------------

* :dfn:`Parent unit`

   .. code:: Ada

      package Complex is
        type Number is private;
        function "*" (Left, Right : Number) return Number;
        function "/" (Left, Right : Number) return Number;
        function "+" (Left, Right : Number) return Number;
        function "-" (Left, Right : Number) return Number;
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
     procedure Put (C : in Number) is
     begin
       Ada.Text_IO.Put (As_String (C));
     end Put;
     function As_String (C : Number) return String is
     begin
       -- Real_Part and Imaginary_Part are
       -- visible to child's body
       return "(" & Float'Image (C.Real_Part) & ", " &
              Float'Image (C.Imaginary_Part) & ")";
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
     procedure Dump (File               : File_Descriptor;
                      Requested_Location : System.Address;
                      Requested_Size     : Interfaces.C.Size_T);
     ...
   end OS.Mem_Mgmt;

   package OS.Files is
     ...
     function Open (Device : Interfaces.C.char_array;
                     Permission : Permissions := S_IRWXO)
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
`with` Clauses for Ancestors Are Implicit
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
         C : Integer := A;
       end Parent.Child;

-------------------------------------------
 `with` Clauses for Siblings Are Required
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

Which is (are) **NOT** legal initialization(s) of ``Child_Object``?

   A. :answermono:`Parent.Parent_Object + Parent.Sibling.Sibling_Object`
   B. :answermono:`Parent_Object + Sibling.Sibling_Object`
   C. :answermono:`Parent_Object + Sibling_Object`
   D. ``None of the above``

.. container:: animate

   A, B, and C are illegal because there is no reference to package
   :ada:`Parent.Sibling` (the reference to :ada:`Parent` is implied by the
   hierarchy). If :ada:`Parent.Child` had ":ada:`with Parent.Sibling;`", then
   A and B would be legal, but C would still be incorrect because there is
   no implied reference to a sibling.

