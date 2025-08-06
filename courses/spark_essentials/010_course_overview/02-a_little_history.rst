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

  - Formal **verification** of source code

* What if we had a verifying compiler?

  - Check correctness at **compile time**
  - Perform **exhaustive** checking
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

* Our ability to deliver automatic formal verification **critically** depends on
  the **language** that is being analyzed.

|

* Most languages were **not** designed with formal verification as a primary design
  goal.

---------------------------
Formal Verification Goals
---------------------------

* Ideally we would like static verification to be:

  - Deep (tells you something **useful**)
  - Sound (with **no false negatives**)
  - Fast (tells you **now**)
  - Precise (with as few false alarms/positives as possible)
  - Modular (analyzes modules in parallel)
  - Constructive (works on incomplete programs)

|

* SPARK is designed with these goals in mind. Since the eighties!

  - But the language and tools have evolved considerably...

.. container:: speakernote

   "Since the 80ies!" does not mean it is dusty technology!

   We're actually very proud to know what and why we are doing with SPARK over
   such a long time. This is not something anyone can build
   overnight. Technically it has been infeasible, but not really anymore...

