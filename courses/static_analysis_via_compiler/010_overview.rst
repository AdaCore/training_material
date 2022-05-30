**********
Overview
**********

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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

===================
About This Course
===================

--------
Styles
--------

* :dfn:`This` is a definition
* :filename:`this/is/a.path`
* :ada:`code is highlighted`
* :command:`commands are emphasised --like-this`

=============================
Static Analyis Via Compiler
=============================

--------------
Introduction
--------------

+ GNAT can be configured to perform static analysis

  + Warnings enabled via compiler switches

+ GNAT can be told that a subset of the language will be adhered to by the source code

  + Via language-defined :ada:`pragma Restrictions`
  + Affects code generation and run-time library candidates
  + Useful for certification

+ GNAT's analysis is extensive, but not without limitations

  + A compiler rather than a static analyzer
  + :toolname:`CodePeer` will be used as a counter-example
