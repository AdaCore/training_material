==================================
Supplementary Resource: Inline ASM
==================================

-----------------------
Calling Assembly Code
-----------------------

* Calling assembly code is a vendor-specific extension
* GNAT allows passing assembly with :ada:`System.Machine_Code.ASM`

   - Handled by the linker directly

* The developer is responsible for mapping variables on temporaries or registers
* See documentation

   - GNAT RM 13.1 Machine Code Insertion
   - GCC UG 6.39 Assembler Instructions with C Expression Operands

------------------
Simple Statement
------------------

* Instruction without inputs/outputs

   .. code:: Ada

      Asm ("halt", Volatile => True);

   - You may specify :ada:`Volatile` to avoid compiler optimizations
   - In general, keep it False unless it created issues

* You can group several instructions

   .. code:: Ada

      Asm ("nop" & ASCII.LF & ASCII.HT
           & "nop", Volatile => True);
      Asm ("nop; nop", Volatile => True);

* The compiler doesn't check the assembly, only the assembler will

   - Error message might be difficult to read

----------
Operands
----------

* It is often useful to have inputs or outputs...

   - :ada:`Asm_Input` and :ada:`Asm_Output` attributes on types

.. image:: annotated_assembly_statement.png
   :width: 85%

-----------------------------------------
Mapping Inputs / Outputs on Temporaries
-----------------------------------------

.. code:: Ada

  Asm (<script referencing $<input> >,
       Inputs  => ({<type>'Asm_Input (<constraint>,
                                       <variable>)}),
       Outputs => ({<type>'Asm_Output (<constraint>,
                                        <variable>)});

* **assembly script** containing assembly instructions + references to registers and temporaries
* **constraint** specifies how variable can be mapped on memory (see documentation for full details)

 .. list-table::
   :header-rows: 1
   :stub-columns: 1

   * - Constraint

     - Meaning

   * - R

     - General purpose register

   * - M

     - Memory

   * - F

     - Floating-point register

   * - I

     - A constant

   * - g

     - global (on x86)

   * - a

     - eax (on x86)

------------
Main Rules
------------

* No control flow between assembler statements

   - Use Ada control flow statement
   - Or use control flow within one statement

* Avoid using fixed registers

   - Makes compiler's life more difficult
   - Let the compiler choose registers
   - You should correctly describe register constraints

* On x86, the assembler uses ``AT&T`` convention

   - First operand is source, second is destination

* See your toolchain's ``as`` assembler manual for syntax

-------------------------------------
Volatile and Clobber ASM Parameters
-------------------------------------

* :ada:`Volatile` |rightarrow| :ada:`True` deactivates optimizations with regards to suppressed instructions
* :ada:`Clobber` |rightarrow| :ada:`"reg1, reg2, ..."` contains the list of registers considered to be "destroyed" by the use of the ASM call

   - ``memory`` if the memory is accessed

      + Compiler won't use memory cache in registers across the instruction

   - ``cc`` if flags might have changed

-----------------------------------
Instruction Counter Example (x86)
-----------------------------------

.. code:: Ada

   with System.Machine_Code; use System.Machine_Code;
   with Ada.Text_IO;         use Ada.Text_IO;
   with Interfaces;          use Interfaces;
   procedure Main is
      Low   : Unsigned_32;
      High  : Unsigned_32;
      Value : Unsigned_64;
      use ASCII;
   begin
      Asm ("rdtsc" & LF,
           Outputs =>
              (Unsigned_32'Asm_Output ("=g", Low),
               Unsigned_32'Asm_Output ("=a", High)),
           Volatile => True);
      Values := Unsigned_64 (Low) +
                Unsigned_64 (High) * 2 ** 32;
      Put_Line (Values'Image);
   end Main;

----------------------------------
Reading a Machine Register (ppc)
----------------------------------

.. code:: Ada

   function Get_MSR return MSR_Type is
      Res : MSR_Type;
   begin
      Asm ("mfmsr %0",
           Outputs => MSR_Type'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_MSR;
   generic
       Spr : Natural;
    function Get_Spr return Unsigned_32;
    function Get_Spr return Unsigned_32 is
       Res : Unsigned_32;
    begin
       Asm ("mfspr %0,%1",
            Inputs => Natural'Asm_Input ("K", Spr),
            Outputs => Unsigned_32'Asm_Output ("=r", Res),
            Volatile => True);
       return Res;
    end Get_Spr;
    function Get_Pir is new Get_Spr (286);

----------------------------------
Writing a Machine Register (ppc)
----------------------------------

.. code:: Ada

   generic
      Spr : Natural;
   procedure Set_Spr (V : Unsigned_32);
   procedure Set_Spr (V : Unsigned_32) is
   begin
      Asm ("mtspr %0,%1",
           Inputs => (Natural'Asm_Input ("K", Spr),
                      Unsigned_32'Asm_Input ("r", V)));
   end Set_Spr;
