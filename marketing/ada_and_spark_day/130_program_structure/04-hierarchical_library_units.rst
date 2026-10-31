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

  - Can extend packages with visibility to parent private section
  - Extensions do not require recompilation of parent unit
  - Visibility of parent's private section is protected

* Directly support subsystems

  - Extensions all have the same ancestor *root* name

-------------------------------
Visibility Across a Hierarchy
-------------------------------

**TBD** hierarchical_visibility_1.svg

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

.. code:: Ada

   with Ada.Text_IO;
   package body Complex.Utils is
     function As_String (Item : Number) return String is
     begin
       -- Real_Part and Imaginary_Part are
       -- visible to child's body
       return "(" &
              Item.Real_Part'Image &
              ", " &
              Item.Imaginary_Part'Image &
              ")";
     end As_String;
   ...
   end Complex.Utils;

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
