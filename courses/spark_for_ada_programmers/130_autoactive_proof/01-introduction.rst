==============
Introduction
==============

-------------------------
Not All Proofs Are Easy
-------------------------

* correct spec + correct code |rightarrow| proof?

* We saw already limitations of automatic provers:

  - Arithmetic - non-linear and mixed arithmetic
  - Quantifiers - existential quantifiers and induction
  - Proof context - may become too large

* :dfn:`Auto-active proof` overcomes these limitations

  - Based on **automatic** provers
  - Using human **interaction**

* Akin to *developing the proof* like we develop code

  - Still much lower effort than required in proof assistants (Coq, Lean,
    Isabelle...)
  - Special code supporting the proof is called :dfn:`ghost code`

-------------------------------
Investigating Unproved Checks
-------------------------------

* Maybe spec is incorrect? Maybe code is incorrect? Or both?

* Need to investigate unproved checks

  - Easiest way is to get runtime failure in spec or code

    + Test the code+spec with assertions enabled!
    + Then debug with the usual debugging tools

  - Increase the proof effort

    + More provers and time to attempt proof

  - Break down property to prove into easier ones

    + Add intermediate assertions
    + Extract proof of a property in a lemma

* Need to understand the messages output by :toolname:`GNATprove`!

  - Tool tries to help you help it

-----------------
The Proof Cycle
-----------------

.. image:: fortify_analyze_prove_cycle.png

.. container:: speakernote

   For SPARK (or other verification systems users) the basic process is the same, but details vary

