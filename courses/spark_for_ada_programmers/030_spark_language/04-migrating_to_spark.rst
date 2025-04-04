====================
Migrating to SPARK
====================

-----------------------------
Migrating From Ada to SPARK
-----------------------------

* Analyzing the Ada code will point to SPARK violations
* First goal is to reach **Stone level**: Valid SPARK
* Violation: functions with side-effects

  - Fix: add aspect :ada:`Side_Effects` to functions, move calls to assignments

* Violation: pointers do not respect ownership

  - Fix: change types and code to respect ownership

* Violation: illegal use of (volatile) variables inside expressions or
  functions

  - Fix: introduce temporaries, mark functions as volatile

* Define a SPARK interface for a unit in Ada

  - Details discussed in the course on SPARK Boundary

----------------------------
Adoption Guidance Document
----------------------------

.. container:: columns

 .. container:: column

    * Based on adoption experience
    * Proposes adoption levels
    * For every level, presents:

       - Benefits, impact on process, costs, and limitations
       - Setup and tool **usage**
       - **Messages** issued by the tool
       - **Remediation** solutions

 .. container:: column

    .. image:: thales_adoption_manual.png
       :width: 100%

---------------------------
Migrating From C to SPARK
---------------------------

* Same recommendations as when migrating from C to Ada
* Even more important to use appropriate types

  - private types as much as possible (e.g. private type for flags with
    constants and boolean operator instead of modular type)

  - enumerations instead of :code:`int`
  - ranges on scalar types
  - non-null access types
  - type predicates

* Special attention on the use of pointers

  - C uses pointers **everywhere**
  - Better to use parameter modes :ada:`out` and :ada:`in out` and array
    types in Ada

  - Choose between **different access types** in SPARK, with different semantics

    + Details discussed in the course on Pointer Programs

