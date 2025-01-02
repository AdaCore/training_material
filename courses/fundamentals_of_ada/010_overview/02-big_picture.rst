=============
Big Picture
=============

---------------------------------------
Language Structure (Ada95 and Onward)
---------------------------------------

* **Required** :dfn:`Core` implementation

   - Reference Manual (RM) sections 1 :math:`\rightarrow` 13
   - Predefined Language Environment (Annex A)
   - Interface to Other Languages (Annex B)
   - Obsolescent Features (Annex J)

* Optional :dfn:`Specialized Needs Annexes`

   - No additional syntax
   - Systems Programming (C)
   - Real-Time Systems (D)
   - Distributed Systems (E)
   - Information Systems (F)
   - Numerics (G)
   - High-Integrity Systems (H)

-----------------------------
Core Language Content (1/3)
-----------------------------

* Types

  * Language-defined types, including string
  * User-defined types
  * Static types keep things consistent
  * Strong types enforces constraints

* Subprograms

  * Syntax differs between *values* and *actions*
  * :ada:`function` for *value* and :ada:`procedure` for *action*
  * Overloading of names allowed

* Dynamic memory management

  * :dfn:`access type` for abstraction of pointers
  * Access to static memory, allocated objects, subprograms
  * Accesses are **checked** (unless otherwise requested)

* Packages

  * Grouping of related entities
  * Separation of concerns
  * Information hiding

-----------------------------
Core Language Content (2/3)
-----------------------------

* Exceptions

  * Dealing with **errors**, **unexpected** events
  * Separate error-handling code from logic

* Generic Units

    * Code templates
    * Extensive parameterization for customization

* Object-Oriented Programming

  * Inheritance
  * Run-time polymorphism
  * Dynamic **dispatching**

* Contract-Based Programming

  * Pre- and post-conditions on subprograms

    * Formalizes specifications

  * Type invariants and predicates

    * Complex contracts on type definitions

-----------------------------
Core Language Content (3/3)
-----------------------------

* Language-Based Concurrency

  * Explicit interactions
  * Run-time handling
  * Portable

* Low Level Programming

  * Define representation of types
  * Storage pools definition
  * Foreign language integration

------------------------------
Language Examination Summary
------------------------------

* Unique capabilities
* Three main goals

   - **Reliability**, maintainability
   - Programming as a **human** activity
   - Efficiency

* Easy-to-use

   - ...and hard to misuse
   - Very **few pitfalls** and exceptions

-----------------------------------
So Why Isn't Ada Used Everywhere?
-----------------------------------

.. container:: columns

 .. container:: column

    * "... in all matters of opinion our adversaries are insane"

       - *Mark Twain*

 .. container:: column

    .. image:: mark_twain.jpeg

