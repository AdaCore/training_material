======================
Limitations of Proof
======================

---------------------------
Functional Specifications
---------------------------

* **Non-functional** specifications **cannot** be expressed as contracts

   - Time or space complexity
   - Timing properties for scheduling
   - Call sequences

* But **automatons** can be encoded as contracts

   - Being in a given state is a functional property
   - Can use normal queries

     + e.g. contracts on :filename:`Ada.Text_IO` use :ada:`Is_Open`

   - Or ghost imported functions that cannot be executed

     + When query cannot be expressed in the code

-----------------------------------------------
Limitations of Automatic Provers - Arithmetic
-----------------------------------------------

* Provers struggle with non-linear arithmetic

   - Use of multiplication, division, :ada:`mod`, :ada:`rem`
   - e.g. monotonicity of division on positive values
   - Solution: use **lemmas** from the SPARK Lemma Library

* Provers struggle with mixed arithmetic

   - Mix of signed and modular integers
   - Mix of integers and floats
   - Solution: define lemmas for **elementary properties**

------------------------------------------------
Limitations of Automatic Provers - Quantifiers
------------------------------------------------

* Quantified expressions express property over a **collection**

   - Universal: :ada:`(for all I in T'Range => T(I) /= 0)`
   - Existential: :ada:`(for some I in T'Range => T(I) /= 0)`

* Provers struggle with **existential**

   - Need to exhibit a :dfn:`witness` that satisfies the property
   - Solution: define a function that computes the witness

     - i.e. a function that checks :ada:`T(X) /= 0`

* Provers cannot **reason inductively**

   - Inductive reasoning deduces a property over integer :ada:`I`

     + If it can be proved for :ada:`I = 0`
     + If it can be proved for :ada:`I+1` from the property for :ada:`I`

   - Solution: lead the prover to this reasoning with a **loop**

--------------------------------------------------
Limitations of Automatic Provers - Proof Context
--------------------------------------------------

* Proof context for a check in a subprogram :ada:`S` is:

  - The contracts of all subprograms called by :ada:`S`
  - The body of :ada:`S` prior to the check
  - The logical modeling of all entities used in :ada:`S`

* Proof context can become **too large**

  - Thousands of lines in the VC
  - This can make the VC unprovable, or hard to prove

* Various solutions to reduce the proof context

  - Split the body of :ada:`S` in smaller subprograms
  - Extract **properties of interest** in lemmas
  - Use special SPARK features

    + Pragma :ada:`Assert_And_Cut`
    + SPARK Library :ada:`SPARK.Cut_Operations`
    + SPARK annotation :ada:`Hide_Info`
      
-----------------------
Cost/Benefit Analysis
-----------------------

* Not all provable properties are worth proving!
* Difficulty of proof (cost) not correlated with benefit
* e.g. proving that a sorting algorithm preserves the components

   - Trivial by review if the only operation is :ada:`Swap`
   - May require many **annotations** for proof

* Functional correctness of complex algorithms is **costly**

   - Specifications can be larger than code
   - Annotations typically much larger than code (:math:`\times` 10)

---------------------------
Dealing with False Alarms
---------------------------

* Check messages can be justified with pragma :ada:`Annotate`

  .. code:: Ada

     pragma Annotate (GNATprove, Category, Pattern, Reason);

  - :ada:`GNATprove` is a fixed identifier
  - :ada:`Category` is one of :ada:`False_Positive` or :ada:`Intentional`

    + :ada:`False_Positive`: check cannot fail
    + :ada:`Intentional`: check can fail but is not a bug

  - :ada:`Pattern` is a substring of the check message

    + Asterisks :ada:`*` match zero or more characters in the message

  - :ada:`Reason` is a string literal for reviews

    + Reason is repeated in output with switch :command:`--report=all` and in
      analysis summary file :filename:`gnatprove.out`

* Justification inserted immediately after the check message location

  - Or at the beginning of a scope

    + Applies to all the scope
    + Generally used when not suitable after the check message location

