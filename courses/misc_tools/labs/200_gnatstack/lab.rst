---------------------------
:toolname:`GNATstack` Lab
---------------------------

* We are going to perform stack analysis on some source code examples

   * Although this is called a lab, it's more like a walk-through!

* Copy the :filename:`200_gnatstack` lab folder from the course materials location

* Contents of the folder:

  * :filename:`simple` - folder containing a simple main procedure
  * :filename:`complicated` - folder containing multiple main procedures and some other packages

.. note:: We use animation - if you don't know the answer, Page Down should give it to you

---------------------------------------------
Getting Familiar with :toolname:`GNATstack`
---------------------------------------------

1. Open a command prompt window and navigate into the folder :filename:`simple`

2. Build the executable :ada:`main_unit`, making sure to generate call-graph information

   * Don't forget to use the light-tasking runtime (switch :command:`--RTS=light`)

.. container:: animate 2-

   :command:`gprbuild --RTS=light main_unit.adb -cargs -fcallgraph-info=su`

3. Perform the stack analysis

.. container:: animate 3-

   :command:`gnatstack *.ci`

   ::

      main : total 224 bytes
       +-> main
       +-> main_unit
       +-> main_unit.inverse

   The numbers may be different, but the calls should match

---------------------------
Adding Source Information
---------------------------

To see where the total number of bytes comes from, run the analysis in :dfn:`verbose` mode

.. container:: animate 2-

   :command:`gnatstack -v *.ci`

---------------------
Verbose Mode Output
---------------------

Verbose mode also shows full path to the source

.. container:: latex_environment small

   ::

      +-> main at main:b__main_unit.adb:20:4 : 64 bytes
      +-> main_unit at Main_Unit:L:\\main_unit.adb:1:1,
          ada_main_program:b__main_unit.adb:17:14 : 96 bytes
      +-> main_unit.inverse at Inverse:L:\\main_unit.adb:4:4 : 64 bytes

-----------------------------
Working with Multiple Mains
-----------------------------

1. Open a command prompt window and navigate into the folder :filename:`complicated`

2. Examine the GNAT Project file and notice the following:

   * The :command:`-fcallgraph-info=su` switch is specified in the :ada:`Compiler` package
   * All main subprograms are specified using :ada:`for Main`

      * Otherwise :command:`gprbuild` does not know what executables to build

   * The runtime is specified using :ada:`for Runtime`

3. Build all the executables using the included :filename:`default.gpr`

   :command:`gprbuild -P default.gpr`

--------------------------
Recursive Calls (Cycles)
--------------------------

.. code:: Ada

   procedure Odd (Number : in out Integer) is
   begin
      Number := Number - 1;
      if Number > 0 then
         Cycles (Number);
      end if;
   end Odd;

   procedure Even (Number : in out Integer) is
   begin
      Number := Number - 2;
      if Number > 0 then
         Cycles (Number);
      end if;
   end Even;

   procedure Cycles (Number : in out Integer) is
      Half : constant Integer := Number / 2;
   begin
      if Half * 2 = Number then
         Even (Number);
      else
         Odd (Number);
      end if;
   end Cycles;

----------------------
Investigating Cycles
----------------------

.. container:: animate 1-

   1. Perform the stack analysis for :ada:`Cycles_Main`

.. container:: animate 2-

   :command:`gnatstack -e cycles_main *.ci`

   ::

      Worst case analysis is *not* accurate because of cycles, external calls. Use -Wa for details.

      Accumulated stack usage information for entry points

      cycles_main : total 176+? bytes
       +-> cycles_main
       +-> cycles_example.cycles *
       +-> cycles_example.odd *
       +-> <__gnat_last_chance_handler> *

   2. Notice the warning indicating to use :command:`-Wa` for details - try that.

.. container:: animate 3-

   :command:`gnatstack -Wa -e cycles_main *.ci`

   Notice the added information

   ::

      List of reachable cycles:

      <c1> cycles_example.cycles
       +-> cycles_example.cycles
       +-> cycles_example.even
       +-> cycles_example.cycles

      <c2> cycles_example.cycles
       +-> cycles_example.cycles
       +-> cycles_example.odd
       +-> cycles_example.cycles

--------------------------------------
Subprogram Pointers (Indirect Calls)
--------------------------------------

.. code:: Ada

   type Subprogram_Access_T is access procedure
       (A, B :     Integer;
        C    : out Boolean);
   procedure Procedure_One
     (A, B :     Integer;
      C    : out Boolean) is
   begin
      C := A > B;
   end Procedure_One;

   procedure Procedure_Two
     (A, B :     Integer;
      C    : out Boolean) is
   begin
      C := A < B;
   end Procedure_Two;

   Calls : array (Boolean) of Subprogram_Access_T :=
     (Procedure_One'Access,
      Procedure_Two'Access);

   procedure Test (Flag : in out Boolean) is
   begin
      Calls (Flag).all (1, 2, Flag);
   end Test;

------------------------------
Investigating Indirect Calls
------------------------------

.. container:: animate 1-

   1. Perform the stack analysis for :ada:`Indirect_Main`

.. container:: animate 2-

   :command:`gnatstack -e indirect_main *.ci`

   ::

      Worst case analysis is not accurate because of external calls, indirect calls. Use -Wa for details.

      Accumulated stack usage information for entry points

      indirect_main : total 112+? bytes
       +-> indirect_main
       +-> indirect_example.test
       +-> indirect call *

   2. Notice the warning indicating to use :command:`-Wa` for details - try that.

.. container:: animate 3-

   :command:`gnatstack -Wa -e indirect_main *.ci`

   Notice the added information

   ::

      List of reachable external subprograms:

        <__gnat_last_chance_handler>

      List of reachable and unresolved indirect (including dispatching) calls:

        1 indirect call in: indirect_example.test
          at L:\indirect_example.adb:26

----------------------
Using Other Switches
----------------------

If you have time, experiment with some other switches

.. container:: animate 1-

   * Show information for multiple main programs

.. container:: animate 2-

   :command:`gnatstack -e indirect_main,cycles_main *.ci`

.. container:: animate 1-

   * Show target for dispatching calls

.. container:: animate 3-

   :command:`gnatstack -td -e dispatching_main *.ci`
