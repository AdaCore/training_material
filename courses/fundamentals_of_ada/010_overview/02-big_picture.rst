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

-------------------------
*Core* Language Content
-------------------------

* Ada is a **compiled**, **multi-paradigm** language
* With a **static** and **strong** type model

.. container:: columns

 .. container:: column

    * Language-defined types, including string
    * User-defined types
    * Overloading procedures and functions
    * Compile-time visibility control
    * Abstract Data Types (ADT)

 .. container:: column

    * Exceptions
    * Generic units
    * Dynamic memory management
    * Low-level programming
    * Object-Oriented Programming (OOP)
    * Concurrent programming
    * Contract-Based Programming

--------------------------
The Type Model Saves Money
--------------------------

* Shifts fixes and costs to **early phases**


* Cost of an error *during a flight*?

.. image:: relative_cost_to_fix_bugs.jpg
   :height: 50%

-------------
Subprograms
-------------

- Syntax differs between *values* and *actions*
- :ada:`function` for a *value*

.. code:: Ada

  function Is_Leaf (T : Tree) return Boolean

- :ada:`procedure` for an *action*

.. code:: Ada

  procedure Split (T     : in out Tree;
                   Left  : out Tree;
                   Right : out Tree)

* Specification :math:`\neq` Implementation

   .. code:: Ada

      function Is_Leaf (T : Tree) return Boolean;
      function Is_Leaf (T : Tree) return Boolean is
      begin
      ...
      end Is_Leaf;

---------------------------
Dynamic Memory Management
---------------------------

* Raw pointers are error-prone
* Ada **access types** abstract facility

    - Static memory
    - Allocated objects
    - Subprograms

* Accesses are **checked**

    - Unless unchecked mode is used

* Supports user-defined storage managers

    - Storage **pools**

----------
Packages
----------

* Grouping of related entities

   - Subsystems like *Fire Control* and *Navigation*
   - Common processing like *HMI* and *Operating System*

* Separation of concerns

   - Specification :math:`\neq` Implementation
   - Single definition by **designer**
   - Multiple use by **users**

* Information hiding

   - Compiler-enforced **visibility**
   - Powerful **privacy** system

------------
Exceptions
------------

* Dealing with **errors**, **unexpected** events
* Separate error-handling code from logic
* Some flexibility

   - Re-raising
   - Custom messages

---------------
Generic Units
---------------

.. container:: columns

 .. container:: column

    * Code Templates

       - Subprograms
       - Packages

    * Parameterization

       - Strongly typed
       - **Expressive** syntax

 .. container:: column

    .. image:: generic_template_to_instances.png

-----------------------------
Object-Oriented Programming
-----------------------------

* Inheritance
* Run-time polymorphism
* Dynamic **dispatching**
* Abstract types and subprograms
* **Interface** for multiple inheritance

----------------------------
Contract-Based Programming
----------------------------

* Pre- and post-conditions
* Formalizes specifications

   .. code:: Ada

      procedure Pop (S : in out Stack) with
          Pre => not S.Empty, -- Requirement
          Post => not S.Full; -- Guarantee

* Type invariants

   .. code:: Ada

      type Table is private with Invariant => Sorted (Table); -- Guarantee

--------------------------
Language-Based Concurrency
--------------------------

* **Expressive**

    - Close to problem-space
    - Specialized constructs
    - **Explicit** interactions

* **Run-time** handling

    - Maps to OS primitives
    - Several support levels (Ravenscar...)

* **Portable**

   - Source code
   - People
   - OS & Vendors

-----------------------
Low Level Programming
-----------------------

* **Representation** clauses
* Bit-level layouts
* Storage pools definition

    - With access safeties

* Foreign language integration

    - C
    - C++
    - Assembly
    - etc...

* Explicit specifications

    - Expressive
    - Efficient
    - Reasonably portable
    - Abstractions preserved

---------------------------------
Standard Language Environment
---------------------------------

Standardized common API

.. container:: columns

 .. container:: column

    * Types

       - Integer
       - Floating-point
       - Fixed-point
       - Boolean
       - Characters, Strings, Unicode
       - etc...

    * Math

        - Trigonometric
        - Complexes

    * Pseudo-random number generators

 .. container:: column

    * I/O

        - Text
        - Binary (direct / sequential)
        - Files
        - Streams

    * Exceptions

        - Call-stack

    * **Command-line** arguments
    * **Environment** variables
    * **Containers**

        - Vector
        - Map

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

