.. role:: ada(code)
    :language: ada

**********
Overview
**********

==================
A Little History
==================

----------
The Name
----------

* First called DoD-1
* Augusta Ada Byron, "first programmer"

   - Lord Byron's daughter
   - Planned to calculate **Bernouilli's numbers**
   - **First** computer program
   - On **Babbage's Analytical Engine**

* Writing **ADA** is like writing **CPLUSPLUS**
* International Standards Organization standard

   - Updated about every 10 years

--------------------------
Ada Evolution Highlights
--------------------------

.. container:: columns

 .. container:: column
  
    * **Ada 83**

       - Abstract Data Types
       - Modules
       - Concurrency
       - Generics
       - Exceptions

    * **Ada 95**

       - OOP
       - Efficient synchronization
       - Better Access Types
       - Child Packages
       - Annexes

 .. container:: column
  
    * **Ada 2005**

       - Multiple Inheritance
       - Containers
       - Better Limited Types
       - More Real-Time
       - Ravenscar

    * **Ada 2012**

       - Contracts
       - Iterators
       - Flexible Expressions
       - More containers
       - Multi-processor Support
       - More Real-Time

=============
Big Picture
=============

---------------------------------------
Language Structure (Ada95 and Onward)
---------------------------------------

* **Required** *Core* implementation

   - Reference Manual (RM) sections 1 :math:`\rightarrow` 13
   - Predefined Language Environment (Annex A)
   - Foreign Language Interfaces (Annex B)


* Optional *Specialized Needs Annexes*

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

----------------
Ada Type Model
----------------

* **Static** Typing

   - Object type **cannot change**
   - ... but run-time polymorphism available (OOP)

* **Strong** Typing

   - **Compiler-enforced** operations and values
   - **Explicit** conversions for "related" types
   - **Unchecked** conversions possible

* Predefined types
* Application-specific types

    - User-defined
    - Checked at compilation and run-time

------------------------
Weakly-Typed Languages
------------------------

* Conversions are **unchecked**
* Type errors are easy

.. code:: C++

   typedef enum { north, south, east, west } direction ;
   direction heading = north;
   typedef enum { mon, tue, wed, thur, fri, sat, sun } weekday;
   weekday day = wed;

   ...

   day = heading; // typo?
   heading = tue + 3 * south/sun;// what?

--------------------------
Strongly-Typed Languages
--------------------------

* Conversions are **checked**
* Type errors are hard

.. code:: Ada

   type Directions is ( North, South, East, West );
   type Days is ( Mon, Tue, Wed, Thu, Fri, Sat, Sun );

   Heading : Directions := North;
   Day : Days := Wed;
   
   ...

   Day := Heading; -- Compile Error

   Heading := Tue + 3 * South/Sun; -- Compile Error
 
--------------------------
The Type Model Saves Money
--------------------------

* Shifts fixes and costs to **early phases**
* **Cheaper**

    - Cost of an error *during a flight*?

.. image:: ../../images/relative_cost_to_fix.png
   :height: 50%

---------------------------
Type Model Run-Time Costs
---------------------------

* Checks at compilation **and** run-time
* **Same performance** for identical programs

   - Run-time type checks can be disabled
   - Compile-time check is *free*

.. container:: columns

 .. container:: column

   **C**

   .. code:: C++

      int X;
      int Y; // range 1 .. 10
      ...
      if (X > 0 && X < 11)
        Y = X;
      else
        // signal a failure
 
 .. container:: column

   **Ada**

   .. code:: Ada

      X : Integer;
      Y, Z : Integer range 1 .. 10;
      ...
      Y := X;
      Z := Y; -- no check required

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
* Separation of concerns

   - Definition :math:`\neq` usage
   - Single definition by **designer**
   - Multiple use by **users**

* Information hiding

   - Compiler-enforced **visibility**
   - Powerful **privacy** system

-------------------
Package Structure
-------------------

* Declaration view

    - **Can** be referenced by user code
    - Exported types, variables...

* Private view

    - **Cannot** be referenced by user code
    - Exported **representations**

* Implementation view

    - Not exported

---------------------------
Abstract Data Types (ADT)
---------------------------

* **Variables** of the **type** encapsulate the **state**
* Classic definition of an ADT

   - Set of **values**
   - Set of **operations**
   - **Hidden** compile-time **representation**

* Compiler-enforced

   - Check of values and operation
   - Easy for a computer
   - Developer can focus on **earlier** phase: requirements
 
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
  
    .. image:: ../../images/generic_template_to_instances.png
    
-------------------
Stack with Generics
-------------------

.. code:: Ada

   generic
     type Content is ... -- type is factored out
   package Bounded_Stacks is
     type Stack is private;
     procedure Push (This : in out Stack;
                     Item : in     Content);
     procedure Pop (This : in out Stack;
                    Item : out    Content);
     ...
     Max : constant := 100;
   private
     type Contents is array (1 .. Max) of Content;
     type Stack is
       record
         Values : Contents;
         Top    : Integer range 0 .. Max := 0;
     end record;
   end Bounded_Stacks;
 
-----------------------------
Object-Oriented Programming
-----------------------------


* Extension of ADT

    - Sub-types
    - Run-time flexibility

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
    - **Explicit** interractions

* **Run-time** handling

    - Maps to OS primitives
    - Several support levels (Ravenscar...)

* **Portable**

   - Source code
   - People
   - OS & Vendors

-----------------------
Concurrency Mechanisms
-----------------------

* Task

   - **Active**
   - **Rich** API
   - OS threads

* Protected object

   - **Passive**
   - *Monitors* protected data
   - **Restricted** set of operations
   - No thread overhead
   - Very portable

* Object-Oriented

   - Synchronized interfaces
   - Protected objects inheritance

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
    - ect...

* Explicit specifications

    - Expressive
    - Efficient
    - Reasonably portable
    - Abstractions preserved

---------------------------------
**Standard** Language Environment
---------------------------------

* Standardized common API

.. container:: columns

 .. container:: column
  
    * Types

       - Integer
       - Floating-point
       - Fixed-point
       - Boolean
       - Characters, Strings, Unicode
       - ect...

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

   - **Reliability**, maintenability
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
  
    .. image:: ../../images/mark_twain.jpeg
    
=======
Setup
=======

-------------------------
Canonical First Program
-------------------------

.. code:: Ada

   1 with Ada.Text_IO;
   2 -- Everyone's first program
   3 procedure Say_Hello is 
   4 begin
   5   Ada.Text_IO.Put_Line ("Hello, World!");
   6 end Say_Hello;
 
* Line 1 - :ada:`with`  - Package dependency
* Line 2 - :ada:`--` - Comment
* Line 3 - :ada:`Say_Hello` - Subprogram name
* Line 4 - :ada:`begin` - Begin executable code
* Line 5 - :ada:`Ada.Text_IO.Put_Line ()` - Subprogram call
* (cont) - :ada:`"Hello, World!"` - String literal (type-checked)

----------------------------------
"Hello World" Lab - Command Line
----------------------------------

* Use an editor to enter the program shown on the previous slide

   - Use your favorite editor or just gedit/notepad/etc.

* Save and name the file :filename:`say_hello.adb` exactly

   - In a command prompt shell, go to where the new file is located and issue the following command:

      + :command:`gprbuild say_hello`

* In the same shell, invoke the resulting executable:

   - :command:`say_hello` (Windows)
   - :command:`./say_hello` (Linux/Unix)

--------------------------------
"Hello World" Lab - :toolname:`GNAT Studio`
--------------------------------

* Start :toolname:`GNAT Studio` from the command-line or Start Menu

* :menu:`Create new project`

   - Select :menu:`Simple Ada Project` and click :menu:`Next`
   - Fill in a location to to deploy the project
   - Set **main name** to *say_hello* and click :menu:`Apply`

* Expand the **src** level in the Project View and double-click :filename:`say_hello.adb`

   - Replace the code in the file with the program shown on the previous slide

* Execute the program by selecting :menu:`Build` :math:`\rightarrow` :menu:`Project` :math:`\rightarrow` :menu:`Build & Run` :math:`\rightarrow` :menu:`say_hello.adb`

   - Shortcut is the :math:`\blacktriangleright` in the icons bar

* Result should appear in the bottom pane labeled *Run: say_hello.exe*
