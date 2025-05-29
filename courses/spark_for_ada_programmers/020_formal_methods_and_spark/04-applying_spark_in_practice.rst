============================
Applying SPARK in Practice
============================

------------------------------
Levels of Software Assurance
------------------------------

* Various reasons for using SPARK

|

* Levels of software assurance

  1. **Stone level** - valid SPARK

  2. **Bronze level** - initialization and correct data flow

  3. **Silver level** - absence of run-time errors (AoRTE)

  4. **Gold level** - proof of key integrity properties

  5. **Platinum level** - full functional proof of requirements

|

* Higher levels are more costly to achieve

|

* Higher levels build on lower levels

  - Project can decide to move to higher level later

------------------------------------------
Levels of Software Assurance in Pictures
------------------------------------------

.. image:: software_assurance_levels.png

---------------------------
Objectives of Using SPARK
---------------------------

* **Safe** coding standard for critical software

  - Typically achieved at **Stone or Bronze** levels

* Prove absence of run-time errors (:dfn:`AoRTE`)

  - Achieved at **Silver** level

* Prove correct **integration** between components

  - Particular case is correct API usage

* Prove **functional correctness**
* Ensure correct behavior of parameterized software
* Safe **optimization** of run-time checks
* Address data and control coupling
* Ensure portability of programs

.. container:: speakernote

   Details of objectives are in section 8.2 of SPARK UG.

-------------------
Project Scenarios
-------------------

* Maintenance and evolution of existing Ada software

  - Requires migration to SPARK of a part of the codebase
  - Fine-grain control over parts in SPARK or in Ada
  - Can progressively move to higher assurance levels

* New developments in SPARK

  - Either completely in SPARK
  - More often interfacing with other code in Ada/C/C++, etc.

