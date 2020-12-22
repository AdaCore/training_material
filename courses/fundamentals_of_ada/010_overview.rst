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

.. container:: columns

 .. container:: column

    * First called DoD-1
    * Augusta Ada Byron, "first programmer"

       - Lord Byron's daughter
       - Planned to calculate Bernouilli's numbers on Babbage's Analytical Engine
       - first computer program

 .. container:: column

  .. image:: images/ada_lovelace.jpeg


* Writing **A.D.A.** is like writing **C.P.L.U.S.P.L.U.S.**
* International Standards Organization standard

   - Updated about every 10 years

--------------------------
Ada Evolution Highlights
--------------------------

.. container:: columns

 .. container:: column
  
    * **Ada 83**

       - Not standardized
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

   enum { north, south, east, west } direction = north;
   enum { mon, tue, wed, thur, fri, sat, sun } day = wed;

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
The Type Model saves Money
--------------------------

* Shifts fixes and costs to **early phases**: specification, coding
* **Cheaper**: What is the cost of an error *during a flight*?


---------------------------
Type Model Run-Time Costs
---------------------------

* Values checked **at run-time**
* **Same performance** for identical programs

   - Type checks can be disabled
   - Compile-time check is *free*
   - **Unified** error handling

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

* Can be either a ``function`` or a ``procedure``

   - Functions represent values
   - Procedures represent actions

* Are syntactically distinguished

   .. code:: Ada

      function Is_Leaf (T : Tree) return Boolean
      procedure Split (T     : in out Tree;
                       Left  : out Tree;
                       Right : out Tree)
 
* Provide direct syntactic support for separation of specification from implementation

   .. code:: Ada

      function Is_Leaf (T : Tree) return Boolean;
      function Is_Leaf (T : Tree) return Boolean is
      begin
      ...
      end Is_Leaf;
 
---------------------------
Dynamic Memory Management
---------------------------

* Pointers are known to be error-prone

   - Easy to misuse

* Ada defines more abstract facility

   - Called "access types" instead of "pointers"

* Can designate "declared" or "allocated" objects
* Can designate subprograms
* Values always meaningful unless unchecked programming used
* Users can define their own storage managers

----------
Packages
----------

* Modules that group related entities together
* Support abstraction

   - Separate specification from implementation

* Support information hiding

   - Compiler enforces visibility for references by clients

* Isolate implementation decisions

   - Defined in one place, used everywhere by clients

-------------------
Package Structure
-------------------

* Visible part

   - Compiler allows client references

   .. code:: Ada

      package Name is
        -- exported declarations of
        --    types, variables, subprograms ...
      end Name;
   
* Implementation part

   - Compiler prevents client references

   .. code:: Ada

      package body Name is
        -- hidden declarations of
        --    types, variables, subprograms ...
        -- implementations of exported subprograms etc.
      end Name;
 
---------------------------
Abstract Data Types (ADT)
---------------------------

* State is encapsulated within variables of the type
* Classic definition

   - Set of applicable values
   - Set of applicable operations on objects of the type
   - Compile-time hidden representation

* The compiler enforces your application model

   - Allowed values
   - Allowed operations

* Makes the computer work for you

   - Bookkeeping is what it does best!
   - Allows us to focus on "the hard stuff"

-------------------------------
Package Optional Private Part
-------------------------------

.. code:: Ada

   package Name is
     -- exported declarations of
     --    types, variables, subprograms ...
   private
     -- hidden declarations of
     --    types, variables, subprograms ...
   end Name;
   
   package body Name is
     -- hidden declarations of
     --    types, variables, subprograms ...
     -- implementations of exported subprograms etc.
   end Name;
 
---------------
Private Types
---------------

* Directly support Abstract Data Types

   .. code:: Ada
      
      package Bounded_Stacks is
         type Stack is private;
         procedure Push (This : in out Stack;
                         Item : in     Integer);
         procedure Pop (This : in out Stack;
                        Item : out    Integer);
         ...
         Max : constant := 100;
      private
         type Contents is array (1 .. Max) of Integer;
         type Stack is record
            Values : Contents;
            Top : Integer range 0 .. Max := 0;
         end record;
      end Bounded_Stacks;
 
---------------------------------
Corresponding Expression In C++
---------------------------------

.. code:: C++

   #ifndef BOUNDED_STACKS_
   #define BOUNDED_STACKS_
   namespace Bounded_Stacks {
      enum {Max=100};
      class Stack { 
      public:
         Stack();
         void Push (int X);
         void Pop (int& X);
      private:
         int Values[Max];
         int Top;
      }; // Stack
   } // Bounded_Stacks
   #endif
 
------------
Exceptions
------------

* Facilities for dealing with errors or other unexpected situations during execution
* Common in modern languages

   - Represent errors and are raised when necessary
   - Can be handled to express recovery

* Have different syntax from class-based languages

   - Exceptions are not classes in Ada

* Allow flexible manipulation (within limits)

   - Re-raising outside original scope, etc.
   - Attaching messages to occurrences

---------------
Generic Units
---------------

.. container:: columns

 .. container:: column
  
    * Are templates for program units

       - Subprograms
       - Packages

    * Allow parameterization of program units

       - Tailorable components within a strongly typed environment

 .. container:: column
  
    .. image:: ../../images/generic_template_to_instances.png
    
------------------------------
Generic Version of Stack ADT
------------------------------

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

* Next step after "ADT Programming"

   - Abstract Data Types
   - Builds upon ADT concepts and practices

* OOP = ADT Programming plus run-time flexibility
* Directly supported

   - Inheritance
   - Run-time polymorphism
   - Dynamic dispatching
   - Abstract types and subprograms
   - Interface types for multiple inheritance

      + Thread-safe too

----------------------------
Contract-Based Programming
----------------------------

* Pre- and postconditions specify subprogram obligations for caller and implementer

   .. code:: Ada

      procedure P (This : in out Integer) with
          Pre => This < Integer'Last, -- Requirement
          Post => This = This'Old + 1; -- Guarantee
 
* Type Invariants ensure general properties of objects

   .. code:: Ada

      type Table is private with Invariant => Sorted (Table);
 
---------------------------------
Pre- and Postconditions Example
---------------------------------

.. code:: Ada

   package Bounded_Stacks is
     type Stack is private;
     function Empty (This : Stack) return Boolean;
     function Full (This : Stack) return Boolean;
     procedure Push (This : in out Stack;  Value : Content)
       with Pre  => not Full (This),
            Post => not Empty (This) and Top (This) = Value;
     procedure Pop (This : in out Stack;  Value : out Content)
       with Pre  => not Empty (This),
            Post => not Full (This);
     function Top (This : Stack) return Content
     with Pre => not Empty (This);
   private
     ...
   end Bounded_Stacks;
 
-------------------------------------
Language-Based Concurrency Approach
-------------------------------------

* Compile-time checking

   - Interactions
   - Parameter types and modes
   - Interface consistency

* Closer mapping of problem space
* Specific constructs for interactions
* Explicit interactions within source code
* Enhanced portability

   - Source code
   - People
   - Much less dependent upon OS and vendor

----------------------------
Ada Concurrency Mechanisms
----------------------------

* Task objects

   - Provide active threads of control

* Protected objects

   - Passive
   - Essentially "monitors" with high-level condition synchronization
   - Synchronize access to values without thread overhead

* Integrated with OOP

   - Synchronized interfaces
   - Dynamic dispatching to entries and protected subprograms
   - Et cetera

-----------------------
Low Level Programming
-----------------------

* Facilities designed for embedded systems

   - Direct manipulation of hardware
   - Direct interaction with assembly language

* As effective as any high order language

   - Expressive
   - Well-specified
   - Efficient

* Reasonably portable

   - Not all software can or should be absolutely portable!

* Abstraction largely preserved

-------------------------------
Low Level Programming Support
-------------------------------

* Extensive representation queries
* Explicit representation specifications

   - Flexible bit-specific type layouts with guaranteed semantics
   - Size (in bits) for objects
   - Storage requirements for tasks
   - Dynamic storage collection ("heap") sizes for access types
   - Memory locations for individual objects
   - Others...

* Interfacing with other languages

   - FORTRAN, C, Assembly, etc.

* Inline assembly language code insertions

---------------------------------
Predefined Language Environment
---------------------------------

.. container:: columns

 .. container:: column
  
    * Standard types and operations for them

       - Integer, floating- and fixed-point, unsigned
       - Boolean
       - Characters and Strings of different sizes
       - etc.

    * Character handling and string handling routines
    * Elementary numeric functions (sine, cosine, etc.)
    * Pseudo-random number generators

 .. container:: column
  
    * I/O for text, direct/sequential binary, streams
    * Exception information manipulation
    * Command-line argument access
    * Environment variables access and manipulation
    * Standard "containers" data structures library
    * And more...

------------------------------
Language Examination Summary
------------------------------

* A uniquely powerful combination of capabilities
* Designed with three overriding concerns

   - Program reliability and maintenance
   - Programming as a human activity
   - Efficiency

* An easy-to-use language

   - Once you know it!
   - Very few pitfalls

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
 
* Line 1 - *with*  - Notification of dependence on a module
* Line 2 - *--* - Comment
* Line 3 - *Say_Hello* - Subprogram name
* Line 4 - *begin* - Begin executable code
* Line 5 - *Ada.Text_IO.Put_Line* - Subprogram call
* (cont) - *"Hello, World!"* - String literal (type-checked)

----------------------------------
"Hello World" Lab - Command Line
----------------------------------

* Use an editor to enter the program shown on the previous slide

   - Use your favorite editor or just gedit/notepad/etc.

* Save and name the file :filename:`say_hello.adb` exactly

   - In a command prompt shell, go to where the new file is located and issue the following command:

      + :command:`gnatmake say_hello`

* In the same shell, invoke the resulting executable:

   - :command:`say_hello` (Windows)
   - :command:`./say_hello` (Linux/Unix)

--------------------------------
"Hello World" Lab - GNATstudio
--------------------------------

* Start :toolname:`GNATstudio` from the command-line or Start Menu

* :menu:`Create new project`

   - Select :menu:`Simple Ada Project` and click :menu:`Next`
   - Fill in a location to to deploy the project
   - Set **main name** to *say_hello* and click :menu:`Apply`

* Expand the **src** level in the Project View and double-click :filename:`say_hello.adb`

   - Replace the code in the file with the program shown on the previous slide

* Execute the program by selecting :menu:`Build` :math:`\rightarrow` :menu:`Project` :math:`\rightarrow` :menu:`Build & Run` :math:`\rightarrow` :menu:`say_hello.adb`

   - Shortcut is the :math:`\blacktriangleright` in the icons bar

* Result should appear in the bottom pane labeled *Run: say_hello.exe*
