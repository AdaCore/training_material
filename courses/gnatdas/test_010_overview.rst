**********
GNATtest
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

---------------------------
Why Automate the Process?
---------------------------

+ Developing tests is labor-intensive
+ Much of the effort is not specific to the tests

  + Developing the harness and driver

    + How to test generic units, etc.

  + Verifying output is as expected
  + Maintenance and update when new units to be tested

+ Ideally developers should concentrate on the high-value part: the test cases themselves
+ :toolname:`GNATtest` makes that ideal possible

------------------------
What Can Be Automated?
------------------------

.. image:: gnatdas/test_what_can_be_automated.jpg

----------
GNATtest
----------

+ Tool to create unit test framework

  + Creates skeleton for each visible subprogram in packages under consideration

+ Automatic unit test infrastructure generation including

  + Test harness
  + Stub generation
  + Aggregates results from multiple test drivers

----------------
Legal Ada Code
----------------

+ Sources must be compilable

  + Warnings issued otherwise
  + If not, :toolname:`GNATtest` will skip it and continue to any others

+ All source dependencies must be available

  + Those units named in :ada:`with` clauses, transitively
  + Whether or not they are to be analyzed themselves

----------------
Based On AUnit
----------------

+ Unit test framework based on :toolname:`CppUnit for C++`
+ Generates the boilerplate code for test harnesses, suites, and cases needed to use the framework
+ For more information on :toolename:`AUnit` view the series of tutorials created by Daniel Bigelow

  + :url:`http://www.youtube.com/user/DanielRBigelow`
