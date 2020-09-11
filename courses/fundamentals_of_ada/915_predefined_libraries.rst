
**********************
Predefined Libraries
**********************

-------------------------
Ada Top-Level Namespace
-------------------------

* Top-level language-defined packages

   - Standard
   - Ada
   - Interfaces
   - System

* Implementations can add top-level units

   - GNAT

* Binding generators and support for inter-language interfacing can make libraries in other languages accessible to Ada and vice versa
* Full hierarchy (language-defined and GNAT) is browsable from GPS Help menu

---------------------
Ada.* Hierarchy (1)
---------------------

* Character Handling

   - `Ada.Characters`
   - `Ada.Wide_Characters`
   - `Ada.Wide_Wide_Characters`

* String Handling

   - `Ada.Strings`

      + Fixed-length, bounded-length, and unbounded-length strings
      + Sets and mappings
      + Hashing, comparison and encoding
      + Support for `String`, `Wide_String`, `Wide_Wide_String`

* Numerics

   - `Ada.Numerics`
   - `Ada.Numerics.Generic_Elementary_Functions`
   - `Ada.Numerics.Float_Random`

---------------------
Ada.* Hierarchy (2)
---------------------

* Input/Output

   - `Ada.Sequential_IO`
   - `Ada.Direct_IO`
   - `Ada.Storage_IO`

* `Ada.IO_Exceptions`
* Text (Graphical Character) I/O

   - `Ada.Text_IO`
   - `Ada.Wide_Text_IO`
   - `Ada.Wide_Wide_Text_IO`

* Execution Environment Interfacing

   - `Ada.Command_Line`
   - `Ada.Directories`
   - `Ada.Environment_Variables`

---------------------
Ada.* Hierarchy (3)
---------------------

* Containers

   - `Ada.Containers`

* Internationalization

   - `Ada.Locales`

* Many predefined units are intrinsic to the core language, including:

   - `Ada.Exceptions`
   - `Ada.Iterator_Interfaces`
   - `Ada.Finalization`
   - `Ada.Tags`
   - `Ada.Streams`
   - `Ada.Assertions`

* Other predefined units are defined in Annex B (*Interfacing to Other Languages*) and in the **Specialized Needs Annexes**

----------------------
Interfaces Hierarchy
----------------------

* Interfaces

   - Contains standardized numeric types and operators

      + Sized numbers (e.g. `Integer_16`, `Unsigned_64`)
      + Shift functions
      + Rotation functions

* Language-interfaces

   - `Interfaces.C`

      + `Interfaces.C.Extensions`
      + `Interfaces.C.Pointers`
      + `Interfaces.C_Streams`
      + `Interfaces.C.Strings`

   - `Interfaces.COBOL`
   - `Interfaces.Fortran`

* `Interfaces.Packed_Decimal`

   - For converting to IBM packed decimal format

------------------
System Hierarchy
------------------

* Typical System Packages

   - `System`
   - `System.Address_Operations`
   - `System.Address_to_Access_Conversions`

* Hundreds of other packages for standardizing interaction with environment

   - Many are compiler-specific
