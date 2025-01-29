***********************
:toolname:`GNATcheck`
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

------------------------------
:toolname:`GNATcheck` Is...
------------------------------

+ An **automated** coding standards checker
+ Capable of expressing a variety of rules

  + GNAT compiler **warnings and style** checks
  + Language-defined and GNAT-defined **restrictions**
  + Complexity **metrics**
  + **Specific** :toolname:`GNATcheck` rules

+ Qualified to DO-178 in several programs
+ Integrated in :toolname:`GNAT Studio`

--------------------
Required by DO-178
--------------------

   .. image:: gnatcheck/do178_table_a5.jpg

-----------------------------------------------
Conformance to Standards Requirement - DO-178
-----------------------------------------------

.. container:: latex_environment beamercolorbox {blueonorange}

   | 6.3.4 Reviews and Analyses of the Source Code
   | 
   |   d. Conformance to standards
   | 
   |     The objective is to **ensure that the Software Code Standards were followed** during the development of the code, especially **complexity restrictions and code constraints** that would be consistent with the system safety objectives.
   | 
   |     Complexity includes the degree of coupling between software components, the nesting levels for control structures, and the complexity of logical or numeric expressions.
   | 
   |     This analysis also ensures that **deviations to the standards are justified.**

------------------------------------------
:toolname:`GNATcheck` Input Requirements
------------------------------------------

* Can analyze sources that are not legal

  * But may result in false negatives due to missing/incorrect semantic information
  * Switch :command:`check-semantic` can check if sources are legal

* Can analyze standalone files

  * But will not parse dependencies
  * Use a GNAT Project File as input for better analysis
