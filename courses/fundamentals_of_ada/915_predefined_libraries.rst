******************************
Annex - Predefined Libraries
******************************

-------------------------
Ada Top-Level Namespace
-------------------------

* Top-level language-defined packages

   - :ada:`package Standard`
   - :ada:`package Ada`
   - :ada:`package Interfaces`
   - :ada:`package System`

* GNAT adds top-level units

   - :ada:`package GNAT`

* **Binding generators** and support for Foreign-Language Interface (FLI)
* Interoperable with foreign language code
* Full hierarchy can be browsed from :toolname:`GNAT Studio` Help menu

--------------------------------------
:ada:`package Ada.*`: Standard Types
--------------------------------------

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

-----------------------------------
:ada:`package Ada.*`: Environment
-----------------------------------

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

-------------------------------------
:ada:`package Ada.*`: Miscellaneous
-------------------------------------

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

---------------------------
:ada:`package Interfaces`
---------------------------

* Interfaces

   - Fixed width numeric types and operators

      + Sized numbers (e.g. `Integer_16`, `Unsigned_64`)
      + Shift functions
      + Rotation functions

* Language-interfaces

   - :ada:`Interfaces.C`

      + :ada:`Interfaces.C.Extensions`
      + :ada:`Interfaces.C.Pointers`
      + :ada:`Interfaces.C_Streams`
      + :ada:`Interfaces.C.Strings`

   - :ada:`Interfaces.COBOL`
   - :ada:`Interfaces.Fortran`

* :ada:`Interfaces.Packed_Decimal`

   - IBM packed decimal format

-----------------------
:ada:`package System`
-----------------------

* Typical System Packages

   - :ada:`System`
   - :ada:`System.Address_Operations`
   - :ada:`System.Address_to_Access_Conversions`

* **Hundreds** of other packages for standardizing interaction with environment

   - Many are compiler-specific
