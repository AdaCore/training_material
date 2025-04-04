=======
SPARK
=======

--------------------------
SPARK Is a Formal Method
--------------------------

* **Soundness** is the most important requirement (no missing alarms)

|

* Analysis is a **combination of techniques**

  - :dfn:`Flow analysis` is a simple form of modular abstract interpretation
  - :dfn:`Proof` is modular deductive verification

|

* Inside proof, abstract interpretation is used to compute **bounds** on arithmetic
  expressions

  - Based on type bounds information
  - e.g if :ada:`X` is of type :ada:`Natural`
  - Then :ada:`Integer'Last - X` cannot overflow

----------------------------
SPARK Is a Language Subset
----------------------------

* Static analysis is **very tied** to the programming language

  - Strong typing **simplifies** analysis
  - Some language features **improve** analysis precision

    + e.g. first-class arrays with bounds
      :ada:`Table'First` and :ada:`Table'Last`

  - Some language features **degrade** analysis precision

    + e.g. arbitrary aliasing of pointers, dispatching calls in
      OOP

|

* SPARK hits the **sweet spot** for proof

  - Based on strongly typed feature-rich Ada programming language
  - **Restrictions** on Ada features to make proof easier

    1. Simplify user's effort for annotating the code

    2. Simplify the job of automatic provers

|

* "SPARK" originally stands for "SPADE Ada Ratiocinative Kernel"

------------------
History of SPARK
------------------

* *Vintage SPARK* followed Ada revisions

  - SPARK 83 based on Ada 83
  - SPARK 95 based on Ada 95
  - SPARK 2005 based on Ada 2005

|

* Since 2014, *SPARK* is updated annually

  - OO programming added in 2015
  - Concurrency added in 2016
  - Type invariants added in 2017
  - Pointers added in 2019
  - Exceptions added in 2023

