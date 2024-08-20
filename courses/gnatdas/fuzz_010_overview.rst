**********
GNATfuzz
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
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

-------------------
What is GNATfuzz?
-------------------

* Provides :dfn:`fuzz-testing` facilities for Ada projects

* Features include:

   * Identification of subprograms are suitable for automated fuzz-testing

      * Called *automatically fuzzable subprograms*

   * Construction of fuzz-test harnesses to

      * Receive binary encoded data
      * Invoke subprogram under test
      * Report run-time anomalies.

   * Generation of fuzz-test initial test cases

   * Execution of fuzz-testing campaigns

   * Real-time coverage analysis through integration with :toolname:`GNATcover`

* Based on :dfn:`AFL++` open-source fuzzer (:url:`https://github.com/AFLplusplus/AFLplusplus`)

-----------------------
What is Fuzz-Testing?
-----------------------

* Also called :dfn:`fuzzing`

* Automated testing technique

   * Start with initial input test case (:dfn:`corpus`)
   * Automatically (and repeatedly) generate new test cases and run them

* Tests run at high frequency

   * Detect faulty behavior (exceptions, assertion failures, etc)

* :dfn:`Black box` testing - random data generation with no knowledge of code being tested

* :dfn:`Grey box` testing - random data generation, but uses coverage data to reduce number of tests

   * :toolname:`GNATfuzz` uses Grey box testing

-------------------------------
Four Operating Modes GNATfuzz
-------------------------------

.. image:: gnatdas/fuzz_overview.png
