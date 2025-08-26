=============
Big Picture
=============

---------------------------------------
Language Structure (Ada95 and Onward)
---------------------------------------

* **Required** :dfn:`Core` implementation

  * Always present in each compiler/run-time

    - Basic language contents (types, subprograms, packages, etc.)
    - Interface to Other Languages

* Optional :dfn:`Specialized Needs Annexes`

  * No additional syntax
  * May be present or not depending on compiler/run-time

-----------------------
Core Language Content
-----------------------

* Types 

   * Language- and user-defined type definitions

* Subprograms

   * Differentiate between :ada:`function` (values) and :ada:`procedure` (action)

* Packages

   * Grouping of related entities

* Generic Units

   * Code templates

* Language-Based Concurrency

   * Multi-tasking and synchronization

* Exceptions

   * Separation of error handling from logical flow

* Dynamic memory management
* Object-Oriented Programming
* Contract-Based Programming
* Low Level Programming

---------------------------
Specialized Needs Annexes
---------------------------

Ada compilers can also support 

* Real-Time Systems

   * Multi-tasking issues such as priority and timing

* Distributed Systems

   * Multiple partitions as part of a single Ada program

* Numerics

   * Complex arithmetic, improved floating point accuracy, and very large numbers

* High-Integrity Systems
* Information systems

------------------------------
Language Examination Summary
------------------------------

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

