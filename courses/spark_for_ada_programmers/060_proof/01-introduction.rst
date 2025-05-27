==============
Introduction
==============

----------------
What Is Proof?
----------------

* **Second** static analysis performed by :toolname:`GNATprove`

   - Depends on successful flow analysis

* Models the **computation** in a subprogram
* Models **assertions** in a subprogram
* Performs checks and detects **violations**

   - Generates **logical formulas**

     + aka Verification Conditions (VC)
     + aka Proof Obligations (PO)

   - Automatic provers check that the VC is valid (always true)
   - If not, a check message is emitted

---------------
Hoare Triples
---------------

* **Hoare triples** (1969) used to reason about program correctness

   - With pre- and postconditions

* Syntax: ``{P} S {Q}``

   - ``S`` is a program
   - ``P`` and ``Q`` are **predicates**
   - ``P`` is the **precondition**
   - ``Q`` is the **postcondition**

* Meaning of ``{P} S {Q}`` triple:

   - If we start in a state where ``P`` is true and execute ``S``, then ``S``
     will terminate in a state where ``Q`` is true.

----------------------
Quiz - Hoare Triples
----------------------

Which one of these is **invalid**?

   A. ``{ X >= 3 } Y := X – 1 { Y >= 0 }``
   B. ``{ X >= 3 } Y := X – 1 { Y = X – 1 }``
   C. ``{ False } Y := X – 1 { Y = X }``
   D. :answermono:`{ X >= 3 } Y := X – 1 { Y >= 3 }`
   E. ``{ X >= 3 } Y := X – 1 { True }``

.. container:: animate

   Explanations

   A. :ada:`Y >= 2` entails :ada:`Y >= 0`
   B. This is true independent of the precondition.
   C. This is true independent of the postcondition.
   D. **Invalid**: :ada:`Y >= 2` does not entail :ada:`Y >= 3`
   E. This is true independent of the precondition.

-----------------------------------------
VC Generation - Strongest Postcondition
-----------------------------------------

* VC are generated using a *Strongest Postcondition Calculus*
* The strongest postcondition ``Q`` for a program ``S`` and a precondition
  ``P`` is such that:

  - ``{P} S {Q}`` is a valid Hoare triple
  - For every valid Hoare triple ``{P} S {Q'}``, ``Q`` is **stronger** than ``Q'``,
    i.e. ``Q`` implies ``Q'``

* The strongest postcondition **summarizes** what is known at any program point
* The strongest postcondition is computed through a *predicate transformer*

  - Information is **propagated** from the precondition
  - VCs are generated each time a **check** is encountered

.. note::

   In this case **strong** indicates *more strict*

--------------------------------
Quiz - Strongest Postcondition
--------------------------------

Which one of these has a **Strongest Postcondition**?

   A. ``{ X >= 3 } Y := X – 1 { Y >= 0 }``
   B. ``{ X >= 3 } Y := X – 1 { Y = X – 1 }``
   C. ``{ X >= 3 } Y := X – 1 { Y >= 2 }``
   D. :answermono:`{ X >= 3 } Y := X – 1 { Y = X – 1 and Y >= 2 }`
   E. :answermono:`{ X >= 3 } Y := X – 1 { Y = X – 1 and X >= 3 }`

.. container:: animate

   Explanations

   A. Information about :ada:`X` is lost.
   B. Information about :ada:`X` is lost.
   C. Information about :ada:`X` is lost.
   D. Correct
   E. Correct (equivalent to answer D)

