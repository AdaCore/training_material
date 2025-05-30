================
Formal Methods
================

----------------
Formal Methods
----------------

* Mathematical techniques applied to the development or verification of
  software

  - :dfn:`Heavyweight formal methods` expose the maths to users
  - :dfn:`Lightweight formal methods` hide the maths from users

* Industrially usable formal methods

  - Are applicable to **existing** development **artifacts** (models, code, etc.)
  - Are automated and integrated in **existing processes**
  - Provide value for **certification**
  - Explicitly **encouraged** by some standards

     + Railway (EN 50128)
     + Avionics (DO-178C + DO-333 Formal Methods Supplement)
     + Security (Common Criteria)

-----------------------------
Static Analysis of Programs
-----------------------------

* :dfn:`Abstract interpretation` (AbsInt)

  - AbsInt analyzes an **abstraction** of the program

* :dfn:`Symbolic execution` (SymExe) and :dfn:`bounded model checking` (BMC)

  - Both analyze possible traces of execution of the program
  - SymExe explores traces **one by one**
  - BMC explores traces **all at once**

* :dfn:`Deductive verification` (Proof)

  - Proof analyzes functions **against their specification**

* Static analysis is a formal method when it is :dfn:`sound`

  - Soundness means no missing alarms

* All techniques have different costs and benefits

--------------------------------------
Goals of Static Analysis of Programs
--------------------------------------

* **Automation** is better with AbsInt and SymExe/BMC

  - Proof incurs the cost of writing specification of functions

|

* **Precision** is better with SymExe/BMC and Proof

  - Automatic provers are **more powerful** than abstract domains
  - SymExe/BMC explore infinitely many traces

    + Limit the exploration to a subset of traces

|

* **Soundness** is better with AbsInt and Proof

  - Soundness is not missing alarms (aka :dfn:`false negatives`)
  - AbsInt may cause false alarms (aka :dfn:`false positives`)
  - Sound handling of loops and recursion in AbsInt and Proof

---------------------------------------------
Capabilities of Static Analysis of Programs
---------------------------------------------

* **Modularity** is the ability to analyze a partial program

  - Most programs are partial

    + Libraries themselves
    + Use of external libraries
    + Program during development

  - Proof is inherently modular

|

* **Speed** of the analysis drives usage

  - Unsound analysis can be much faster than sound one
  - For sound analysis, modular analysis is faster

|

* **Usage** depends on capabilities

  - Fast analysis with no false alarms is better for :dfn:`bug-finding`
  - Modular analysis with no missing alarms is better for :dfn:`formal verification`

---------------------------------------
Comparing Techniques on Simple Code
---------------------------------------

* Consider a simple loop-based procedure

.. code:: ada

   procedure Reset (T : in out Table; A, B : Index) is
   begin
      for Idx in A .. B loop
         T(Idx) := 0;
      end loop;
   end;

* :ada:`T(Idx)` is safe |equivalent| :ada:`Idx in T'Range`
* As a result of calling :ada:`Reset`:

  - Array :ada:`T` is initialized between indexes :ada:`A` and :ada:`B`
  - Array :ada:`T` has value zero between indexes :ada:`A` and :ada:`B`

-------------------------
Abstract Interpretation
-------------------------

* :ada:`Reset` is analyzed in the context of each of its calls

  - If the values of :ada:`Table`, :ada:`A`, :ada:`B` are precise enough,
    AbsInt can deduce that :ada:`Idx in T'Range`

  - Otherwise, an **alarm** is emitted (for sound analysis)

|

* Initialization and value of individual array cells is **not** tracked

  - The assignment to a cell is a :dfn:`weak update`

    + The abstract value for the whole array now includes value zero
    + ... but is also possibly uninitialized or keeps a previous value

  - After the call to :ada:`Reset`, the analysis does **not** know that :ada:`T`
    is initialized with value zero between indexes :ada:`A` and :ada:`B`

-----------------------------------------------
Symbolic Execution and Bounded Model Checking
-----------------------------------------------

* :ada:`Reset` is analyzed in the context of **program traces**

  - If the values of :ada:`A` and :ada:`B` are *close enough*, SymExe/BMC can
    analyze all loop iterations and deduce that :ada:`Idx in T'Range`

  - Otherwise, an **alarm** is emitted (for sound analysis)

|

* Analysis of loops is limited to few iterations (same for recursion)

  - The other iterations are ignored or approximated, so the value of :ada:`T`
    is **lost**

  - After the call to :ada:`Reset`, the analysis does **not** know that :ada:`T`
    is initialized with value zero between indexes :ada:`A` and :ada:`B`

------------------------
Deductive Verification
------------------------

* :ada:`Reset` is analyzed in the context of a :dfn:`precondition`

  - Predicate defined by the user which restricts the calling context
  - Proof checks if the precondition entails :ada:`Idx in T'Range`
  - Otherwise, an **alarm** is emitted

* Initialization and value of individual array cells is tracked
* Analysis of loops is based on user-provided :dfn:`loop invariants`

  :ada:`T(A .. Idx)'Initialized and T(A .. Idx) = (A .. Idx => 0)`

* Code after the call to :ada:`Reset` is analyzed in the context of a
  :dfn:`postcondition`

  :ada:`T(A .. B)'Initialized and T(A .. B) = (A .. B => 0)`

  - So the analysis now **knows** that :ada:`T` is initialized with value zero between
    indexes :ada:`A` and :ada:`B`

