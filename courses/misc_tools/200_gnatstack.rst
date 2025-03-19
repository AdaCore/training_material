***********************
:toolname:`GNATstack`
***********************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. role:: rust(code)
    :language: Rust

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==============
Introduction
==============

--------------------------------
Determining Maximum Stack Size
--------------------------------

* :toolname:`GNATstack` statically computes maximum stack space

   * For each application entry point (including tasks)

* **Static** Analysis

   * Analyzes artifacts of compiler (not run-time execution)
   * Computes worst-case stack requirements
   * When no assumptions made, stack size never exceeds analyzed value

---------
Outputs
---------

* Worst-case stack requirements for each entry point

   * Entry points can be deduced from source or specified by user

* Path leading to each scenario

* :dfn:`Visualization of Compiler Graphs (VCG)`

   * File containing complete call tree for application
   * Contains both local and accumulated stack usage

---------------------------
Optional Analysis Outputs
---------------------------

* **Indirect calls** (including dispatching)

   * Number of indirect calls from any subprogram

* **External calls**

   * All subprograms reachable from entry point where no stack/call graph information is available

* **Unbounded frames**

   * All subprograms reachable from entry point with unbounded stack requirements
   * Stack size depends on arguments passed to the subprogram

* **Cycles**

   * Detect call cycles in the call graph
   * Represent potential recursion for possibly unbounded stack consumption

===============================
Running :toolname:`GNATstack`
===============================

----------------------
Example Subprogram
----------------------

.. code:: Ada
   :number-lines: 1

   procedure Main_Unit is
      type Data_Type is array (1 .. 5) of Integer;

      function Inverse (Input : Data_Type) return Data_Type is
         Result : Data_Type;
      begin
         for Index in Data_Type'Range loop
            Result (Index) := Input (Data_Type'Last -
                                     (Index - Data_Type'First));
         end loop;

         return Result;
      end Inverse;

      Data   : Data_Type := (1, 2, 3, 4, 5);
      Result : Data_Type;
   begin
      Result := Inverse (Data);
   end Main_Unit;

--------------------------------------------
Getting Started with :toolname:`GNATstack`
--------------------------------------------

Two parts of performing stack analysis

1. Generation of stack consumption and call-graph information

   :command:`gprbuild --RTS=light main_unit.adb -cargs -fcallgraph-info=su`

   *We use the light runtime to avoid including things like the secondary stack*

2. Analysis and report generation

   :command:`gnatstack *.ci`

Which generates the following report:

.. container:: latex_environment scriptsize

      ::

         Worst case analysis is *not* accurate because of external calls. Use -Wa for details.

         Accumulated stack usage information for entry points

         main : total 224 bytes
          +-> main
          +-> main_unit
          +-> main_unit.inverse

*Note that the actual stack usage can depend on things like runtime, operating system, and compiler version.*

================================
:toolname:`GNATstack` Switches
================================

----------------------------
Execution-Related Switches
----------------------------

:command:`-e main1[,main2[,...]` |rightarrow| Use list of subprograms as entry points

:command:`-a` |rightarrow| Use all subprograms as entry points

:command:`-f filename` |rightarrow| Store callgraph in :filename:`filename`

   * If not specified, stored in :filename:`graph.vcg`

:command:`-P project` |rightarrow| Use GPR file :filename:`project` to find :filename:`*.ci` files

------------------------
Commonly Used Switches
------------------------

:command:`-v` |rightarrow| verbose

   * Show source location for subprogam

:command:`-o=\{a,s\}` |rightarrow| order for displaying call graphs

   * **a** sort alphabetically
   * **s** sort by stack usage (default)

:command:`-t=\{i,d,a\}` - print target for indirect/dispatching calls

   * **i** for indirect calls only
   * **d** for dispatching calls only
   * **a** for both indirect and dispatching calls

=====
Lab
=====

---------------------------
TBD
---------------------------

* include:: labs/200_gnatstack/lab.rst

=========
Summary
=========

------------------------------------
Improving on :toolname:`GNATstack`
------------------------------------

* TBD
