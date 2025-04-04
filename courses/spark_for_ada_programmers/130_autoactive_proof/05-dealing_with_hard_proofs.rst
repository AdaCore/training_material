==========================
Dealing with Hard Proofs
==========================

----------------------------
Reducing the Proof Context
----------------------------

* Large proof context confuses provers

* Lemmas allow reducing the proof context to a minimum

  - Precondition of the lemma

  - Definition of constants, types and subprograms used

* Pragma :ada:`Assert_And_Cut`

  - State property used as cut-point for instructions that follow

  - All variables in context are havoc'ed

  - Proof context may still be large, but fewer ground terms

* SPARK Library :ada:`SPARK.Cut_Operations`

  - Functions ``By`` and ``So`` to chain assertions

  - :ada:`By (A, B)` requires proving ``B``, then ``A`` from ``B``, and leaves only ``A``
    in proof context

  - :ada:`So (A, B)` requires proving ``A``, then ``B`` from ``A``, and leaves both in
    proof context

  - Note: :ada:`A and then B` requires proving separately ``A`` and ``B``

* Annotation :ada:`Hide_Info` and :ada:`Unhide_Info` used to hide/expose
  expression function or private part of package

--------------------
Triggering Provers
--------------------

* SMT provers use *triggers* to instantiate axioms

  - A trigger is a ground term usually appearing in the axiom

  - E.g. :toolname:`GNATprove` generates trigger ``f args`` for axiom defining
    function ``f`` on arguments ``args``

* Annotation ``Inline_For_Proof`` avoids definition of axiom

  - Instead direct definition given for function

  - Applicable to expression function, or function with postcondition
    :ada:`F'Result = ...`

* Call to expression function is inlined when it is a conjunction

  - This facilitates proof in general

  - ... but it removes a potential trigger, making other proofs more difficult!

  - Disable such inlining with an explicit :ada:`Post => True`

-----------------------
Dealing with Equality
-----------------------

* Equality in SPARK :math:`\neq` logical equality

* Equality in SPARK on type :ada:`T` is:

  - The user-defined primitive equality if present

  - The predefined equality otherwise, based on the equality of components:

    - Using the primitive equality on record subcomponents

    - Using the predefined equality on other subcomponents

* Predefined equality on arrays ignores value of bounds

* In general, :ada:`A = B` does not imply ``F (A) = F (B)``

  - Possible to state a lemma proving this property

  - Or use annotation :ada:`Logical_Equal` on equality function

    - :toolname:`GNATprove` checks that this is sound

------------------------
Computing with Provers
------------------------

* Provers not a good fit for computing values

* Proving properties on large constants can be hard

  - E.g. to check validity of configuration data

* Use ghost code to prove intermediate steps

  - Loops without loop invariants of up to 20 iterations are unrolled

  - Calls to local subprograms without contract are inlined

  - Proof by induction using loops with loop invariants

  - Define lemmas for shared proofs

* Alternative is to execute these assertions at runtime

