*******************
Program Structure
*******************

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
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

============================
Hierarchical Library Units
============================

---------------------------
Hierarchical Library Units
---------------------------

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
     procedure Put(C : in Number) is
     begin
       Ada.Text_IO.Put(As_String(C));
     end Put;
     function As_String(C : Number) return String is
     begin
       -- Real_Part and Imaginary_Part are
       -- visible to child's body
       return "(" & Float'Image(C.Real_Part) & ", " &
              Float'Image(C.Imaginary_Part) & ")";
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
