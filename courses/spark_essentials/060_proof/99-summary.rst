=========
Summary
=========

-------
Proof
-------

* Proof uses Strongest Postcondition Calculus to generate formulas
* Formulas aka Verification Conditions (VC) are sent to provers
* Proof detects:

  - Possible run-time errors
  - Possible failure of assertions
  - Violation of functional contracts (:ada:`Pre` and :ada:`Post`)

* Proof allows to reach Silver/Gold/Platinum levels
* Proof is imprecise

  - On non-linear arithmetic and mixed arithmetic
  - On existential quantification and inductive reasoning
  - When the proof context is too large
