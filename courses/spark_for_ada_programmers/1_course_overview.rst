*****************
Course Overview
*****************

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
* :menu:`This` |rightarrow| :menu:`Is` |rightarrow| :menu:`An IDE Menu`

==================
A Little History
==================

--------------------
Motivating Example
--------------------

* Consider these lines of code from the original release of the Tokeneer code
  (demonstrator for the NSA)

   .. code:: Ada

      if Success and then
        (RawDuration * 10 <= Integer(DurationT'Last) and
         RawDuration * 10 >= Integer(DurationT'First))
      then
         Value := DurationT(RawDuration * 10);
      else

* Can you see the problem?

* This error escaped lots of testing and reviews!

.. container:: speakernote

   Overflow can happen before check

------------------------
The Verifying Compiler
------------------------

* Could a compiler find the error we just saw?

  - Formal verification of source code

* What if we had a verifying compiler?

  - Check correctness at compile time
  - Perform exhaustive checking
  - Use types, assertions, and other information in the source code as
    correctness criteria
  - Work in combination with other program development and testing tools

* Grand Challenge for computer science [Hoare 2003]

.. container:: speakernote

   It exists. GNATprove. This is what we will talk about.

-----------------------------------------------
Formal Verification and Programming Languages
-----------------------------------------------

* There is a catch...

|

* Our ability to deliver automatic formal verification critically depends on
  the language that is being analyzed.

|

* Most languages were not designed with formal verification as a primary design
  goal.

---------------------------
Formal Verification Goals
---------------------------

* Ideally we would like static verification to be:

  - Deep (tells you something useful)
  - Sound (with no false negatives)
  - Fast (tells you now)
  - Precise (with as few false alarms/positives as possible)
  - Modular (analyzes modules in parallel)
  - Constructive (works on incomplete programs)

* SPARK is designed with these goals in mind. Since the eighties!

  - But the language and tools have evolved considerably...

.. container:: speakernote

   "Since the 80ies!" does not mean it is dusty technology!

   We're actually very proud to know what and why we are doing with SPARK over
   such a long time. This is not something anyone can build
   overnight. Technically it has been infeasible, but not really anymore...

=======
SPARK
=======

----------------
What is SPARK?
----------------

* SPARK is

   - A programming language
   - A set of formal verification tools
   - A design approach for high-integrity software

* All of the above!

----------------
What is SPARK?
----------------

* Programming language - relationship with Ada:

|

.. image:: ada_vs_spark_venn.png
   :width: 85%

=================
Course Contents
=================

-----------------
Course Outline
-----------------

.. container:: columns

 .. container:: column

    * Introduction to SPARK

      - Formal Methods and SPARK
      - SPARK Language
      - SPARK Tools

    * Formal verification in SPARK

      - Flow Analysis
      - Proof

    * Specifications in SPARK

      - Specification Language
      - Subprogram Contracts
      - Type Contracts

 .. container:: column

    * Advanced Formal Verification

       - Advanced Proof
       - Advanced Flow Analysis

    * Advanced topics

       - Pointer Programs
       - Auto-active Proof
       - State Abstraction

    * SPARK Boundary

--------------
Course Goals
--------------

* What will you do after the course?

  - Be comfortable with the fundamentals of SPARK.
  - Know where to find out more.
  - Let SPARK work for you on your next project?
  - What else?
