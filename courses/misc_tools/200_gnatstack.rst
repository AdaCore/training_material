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

-------------------------------
Running :toolname:`GNATstack`
-------------------------------

:command:`gnatstack [switches] {filename}`

where :filename:`{filename}` can be a package spec or body

* Package spec

  * :toolname:`GNATstack` will generate a package body containing "dummy" bodies for subprograms defined but not completed in the spec

* Package body

  * For any subprogram defined as :ada:`separate` in the package body, a file will be created containing a body for the subprogram

.. note:: Need to specify :command:`--subunits` switch

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

================================
:toolname:`GNATstack` Switches
================================

----------------------------------
Controlling Behavior When Called
----------------------------------

* TBD

---------------------------
TBD
---------------------------

* TBD

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
