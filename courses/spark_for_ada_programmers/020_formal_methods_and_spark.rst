**************************
Formal Methods and SPARK
**************************
.. |rightarrow| replace:: :math:`\rightarrow`
..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

-------------------------
High-Integrity Software
-------------------------

* Has **reliability** as the most important requirement

   - More than cost, time-to-market, etc.

* Must be known to be reliable **before** being deployed

   - With extremely low failure rates, e.g., 1 in 10:superscript:`9` hours (114,080 years)
   - Testing is insufficient and/or infeasible for such rates

* Is **not** necessarily safety-critical

   - Could be *mission*-critical
   - E.g., satellites and remote exploration vehicles
   - E.g., financial systems

* But safety-critical software is also high-integrity software

===============================
Rationale for Formal Analysis
===============================

----------------
Formal Methods
----------------

* **Sufficient** and **cost-effective** approach to show reliability **prior** to deployment
* Are **not** capable of proving entire programs correct

   - We cannot produce a **complete**, correct formal specification
   - Not everything lends itself to formal specification

      + :ada:`generic`, unchecked Ada, :ada:`with Import`...

* Can prove extremely useful properties for portions of programs
* Allow **combined** proof and test

   - You don't have to test that which has been proven
   - Else your tests will pass the first time
   - Language capabilities help with testing

.. container:: speakernote

   A full formal specification of a program involves, in effect, writing the entire program at a different level of abstraction using some formal specification language.  That's difficult and expensive.
   How to specify formally the error messages from a compiler are readable, understandable, useful, etc?

------------------------------------------
Combining Proof and Test - Cost Benefit
------------------------------------------

.. container:: columns

 .. container:: column

    * 80/20 rule holds for both test and proof activities
    * Same area of code is usually not both difficult to prove and difficult to test

 .. container:: column

    .. image:: 80-20_provable_or_testable.png

-------------------------------------
Language Facilitates Proof and Test
-------------------------------------

* Proof contracts are **executable**, not just comments
* Thus proof contracts can be **checked at run-time** with an error raised when a check fails
* The GNAT compiler has specific support options

   - Additional **aliasing detection** beyond Ada RM requirements
   - Additional data **initialization and validity** checks beyond Ada RM requirements
   - See the SPARK User's Guide for more details

* Unit test tool :toolname:`GNATtest` makes testing easier

   - Based on common test framework approach (e.g., **JUnit**)
   - Proof contracts express low-level-requirements!

-------
SPARK
-------

* A formally-defined programming language supporting formal analysis

   - Specifically designed for high-integrity software development

* Plus a set of verification tools to perform those analyses

   - Hides the math from users

* Uses **statically provable** contracts + testing
* Based on Ada

   - Strict subset of Ada **2022**
   - Compiled SPARK is **binary-compatible** with Ada
   - Can be **linked** directly to Ada

--------------------------
The Programming Language
--------------------------

* SPARK design principles:

   - Exclude language features difficult to specify/verify
   - Eliminate sources of ambiguity

* Excluded from SPARK:

   - Aliasing data-structures obtained using access types
   - Expressions (including function calls) with **side-effects**
   - Aliasing of names
   - Backwards goto statements
   - Controlled types
   - Handling of exceptions (raise statements can be used in a program but proof will attempt to show that they cannot be executed)

.. container:: speakernote

   At this point it helps to understand the rationale behind the differences between the SPARK and Ada languages.
   The aim while designing the SPARK subset of Ada was to create the biggest possible subset still amenable to easy specification and sound verification.
   The most notable exclusions include uses of access type and allocators that create aliasing, as well as handling of exceptions, which are both known to increase considerably the amount of required user-written annotations.
   Backwards goto statements and controlled types are also not supported as they introduce non-trivial control flow.
   The two remaining restrictions are side-effects in expressions and aliasing of names, which we will now look at in more detail.

------------------------------
Math Hidden Behind the Tools
------------------------------

* Typical for engineering disciplines

   - Tool abstracts the **heavy-duty** math required
   - User can focus on **abstracted** problems

* This is a simulation of the NASA Space Launch System

   - Shock wave (colored by **pressure**) visible at the front of vehicle
   - Farther back, booster separation-motor plumes are colored by **Mach number**
   - Tool takes care of creating the **visualization**
   - Engineers can focus on issues **visually**

.. image:: rocket_launch_shock_wave.png
   :width: 30%
   :align: center
   :alt: https://www.nasa.gov/ames/image-feature/simulation-sls-booster-separation
   :target: https://www.nasa.gov/ames/image-feature/simulation-sls-booster-separation

===========================
Objectives of Using SPARK
===========================

---------------------------
Objectives of Using SPARK
---------------------------

* Provide a safe coding subset
* Address both data and control coupling
* Prove absence of run-time errors (:dfn:`AoRTE`)
* Prove **functional correctness**
* Prove components integration
* Support combined proof and unit testing

----------------------------------
A Brief Terminology Introduction
----------------------------------

* :dfn:`Contracts` are parts of specifications attached to program entities like subprograms
* :dfn:`Data dependency` contracts specify global data that a subprogram is allowed to **read and/or write**
* :dfn:`Flow dependency` contracts specify how a subprogram's **outputs** depend on its **inputs**
* :dfn:`Precondition` contracts specify caller **constraints**

   - E.g., don't call :ada:`Push` on a full stack

* :dfn:`Postcondition` contracts specify **guarantees** by implementations

   - E.g., after :ada:`Push` returns, the stack is not empty

-------------------------
SPARK Contract Examples
-------------------------

.. code:: Ada

   procedure Push (This : in out Stack;  Item : Element) with
     Pre     => not Full(This),  -- precondition
     Post    => not Empty(This)  -- postcondition
                and then Top_Element(This) = Item
                and then Extent(This) = Extent(This'Old)+1,
     Global  => null,            -- data dependency
     Depends => (This =>+ Item); -- flow dependency

   function Full (This : Stack) return Boolean;
   function Empty (This : Stack) return Boolean;
   function Top_Element (This : Stack) return Element;
   function Extent (This : Stack) return Element_Count;

.. container:: speakernote

   This example is just to give a feel for how contracts look and are expressed. The details will be covered later.

-------------------------------
Objective: Safe Coding Subset
-------------------------------

* Exclusion of **unsafe** features

   - Constructs that **preclude** analysis
   - Constructs that are **likely** to introduce **errors**

      + Functions with side-effects (illustrated later)
      + Others...

* Detection of errors in **earlier** lifecycle phases

   - Use of uninitialized variables
   - Problems due to parameter aliasing (illustrated later)
   - Others...

--------------------------------------
Objective: Data And Control Coupling
--------------------------------------

* :dfn:`Data coupling`

   - "The dependence of a software component on data **not exclusively** under the control of that software component"

* :dfn:`Control coupling`

   - "The manner or degree by which one software component influences the **execution** of another software component"

* Vital to assessing **impact** of code changes
* Vital part of **security measures**, ensuring data only go where intended
* Typically **required** for project **certification**

.. container:: speakernote

   As defined in the avionics standard DO-178.
   See SPARK UG 8.1.7 Address Data and Control Coupling

-----------------------------------
How SPARK Addresses Data Coupling
-----------------------------------

* Global data dependency contracts specify **global inputs and outputs**

    - :ada:`with Global`
    - Identifying data **not exclusively** under unit's control

    .. code:: Ada

        procedure Add_To_Total (X : in Integer) with
           Global => (In_Out => Total);

   - :ada:`Total` is an :ada:`in out` global variable: both **read and written**

* Flow dependency contracts specify exactly how data influence subprogram **output values**

    - :ada:`with Depends`

    .. code:: Ada

        procedure Add_To_Total (X : in Integer) with
           Depends => (Total => (Total, X));

   - Value of output :ada:`Total` depends on inputs :ada:`Total` and :ada:`X`

--------------------------------------
How SPARK Addresses Control Coupling
--------------------------------------

* Flow dependency contracts specify the shared data potentially written by a subprogram

    - Those data values **will** affect the execution (otherwise emit a message)

    .. code:: Ada

        procedure Add_To_Total (X : in Integer) with
           Depends => (Total => (Total, X));

* Functional contracts specify the behavior of a subprogram

    - How it "influences the execution of another software component"

    .. code:: Ada

        procedure Add_To_Total (X : in Integer) with
           Pre => X >= 0 and then Total <= Integer'Last - X;

.. container:: speakernote

   As a global, Total's value must affect some other unit

---------------------------------------
Objective: Absence of Run-Time Errors
---------------------------------------

* SPARK defines a number of run-time checks, and users can define their own too (e.g., through assertions)

   - E.g., valid array indexing

* Absence provides significant benefits!

   - No buffer overflows, for example, so no exploits

* Using proof alone (when possible)
* Using a combination of proof and test

.. container:: speakernote

   SPARK UG: Typically, 95% to 98% of run-time checks can be proved automatically, and the remaining checks can be either verified with manual provers or justified by manual analysis.

----------------------------------------
Why Do We Care About Integer Overflow?
----------------------------------------

* Broken algorithms

   - Most binary search algorithms!

      .. code:: Ada

         Midpoint = (Low + High) / 2

* Security vulnerabilities

   - PHP's `Msg_Receive()` function

      + http://www.securityfocus.com/bid/23236/discuss
      + Allowing attackers to execute arbitrary machine code

   - XDR Library

      + https://www.symantec.com/security_response/vulnerability.jsp?bid=7123
      + Vulnerable to DOS attacks

.. container:: speakernote

   Binary search: what happens when high is close to the limit!

---------------------------------------
Allow Safely Removing Run-Time Checks
---------------------------------------

* These checks ensure proper behavior at run-time

   - Array index checks: no writing into invalid memory locations
   - Division by zero checks: no invalid division operation
   - Others...

* Compiler optimization can remove most checks

   - Depending on the compiler and the way the code is written
   - Some may remain after optimization

* The remainder can be significant, depending on where it manifests in the code
* Removing them removes their protection!

-----------------------------------
Removing Run-Time Checks In SPARK
-----------------------------------

* SPARK allows proof of absence of run-time errors

   - Language-defined and user-defined checks

* Run-time checks appear in sequences of statements, especially those within subprograms
* Proof requires subprogram preconditions to hold

      .. code:: Ada

         procedure Increment (X : in out Integer) with
           Pre => X < Integer'Last;

* Therefore, you must either prove preconditions will hold or enable checks at run-time
* Details discussed in the dedicated AoRTE section...

-----------------------------------
Objective: Functional Correctness
-----------------------------------

* Contracts for functional correctness can be executed...

   - Executable specifications can stand for test oracles
   - If they are checked during integration testing, they may replace unit testing
   - They are more maintainable than comments

* ... as well as formally proven

   - Formal proof verifies the program on all possible inputs
   - Verification can be done earlier in the development, before bodies are implemented

---------------------------------------
Proving Arbitrary Boolean Properties
---------------------------------------

* Proof of functional correctness verifies compliance with a specification
* Specification may extend beyond requirements for program integrity (i.e., absence of run-time errors)

   - Arbitrary properties: derived requirements, etc.

* :toolname:`GNATprove` can verify these additional properties

-----------------------------------------------
Proving Arbitrary Boolean Properties Example
-----------------------------------------------

.. code:: Ada

   --Absence of Run-Time Errors
   function Index (Input  : List;
                   Target : Natural)
                   return Natural with
     Post => Index'Result in 0 | Input'Range;

   --Correctness
   function Index (Input  : List;
                   Target : Natural)
                   return Natural with
     Post => (if not Found (Target, Within => Input) then
                 Index'Result = 0
              else
                 Index'Result in Input'Range
                 and then
                 Input (Index'Result) = Target);

.. container:: speakernote

   For example, to ensure no runtime error is raised when using the result of function Index, it may be enough to know that, whenever it is not 0, then it is in Input's range.
   However, for the program to be meaningful, we may want Index to verify more complex properties. For example, it only returns 0 if E is not in A and that, otherwise, it returns an index of A where E is stored.

---------------------------------
Proof Using Boolean Properties
---------------------------------

.. code:: Ada

   function Index (Input  : List;
                   Target : Natural)
                   return Natural
     with Post =>
            (if not Found (Target, Within => Input)
                then Index'Result = 0
             else Index'Result in Input'Range and then
                  Input (Index'Result) = Target);
   ...
      -- the target (ie array component) of the search
      T : constant Natural := 42;
      -- arbitrary but containing T
      L : constant List := (0, 0, T, others => 1);
      I : Natural;  -- an index value for L
   begin
      I := Index (L, T);
      if I /= 0 then
         pragma Assert (L(I) = T);
         ...
      else
         ...
      end if;
      ...
   end Test;

.. container:: speakernote

   Pragma assert generates two info messages:
   info: index check proved
   info: assertion proved

-------------------------------------------
Objective: Correct Components Integration
-------------------------------------------

* **Component:**

   - A subprogram, a unit, or a set of units

* Even if components are verified individually, their combination may still fail because of unforeseen interactions or design problems
* You can specify exactly the inputs and outputs of each subprogram

   - Data dependency contracts

* You can precisely specify properties about the behavior of each subprogram

   - Precondition and postcondition contracts

* SPARK will attempt to prove those contracts

----------------------------------------
Components Integration: Defensive Code
----------------------------------------

* Code that checks proper caller requirements

   - E.g., cannot call `Push` on a full `Stack` object

* You can replace defensive code with preconditions
* Preconditions help component integration by proving callers adhere to called unit requirements

------------------------------------------------
Components Integration: Defensive Code Example
------------------------------------------------

.. code:: Ada

   function Full (This : Stack)
                  return Boolean;

   procedure Push (This  : in out Stack;
                   Value : Content) is
   begin
      if Full (This) then
         raise Overflow;
      end if;
      ...
   end Push;

   procedure Push (This  : in out Stack;
                   Value : Content)
      with   Pre  => not Full (This),
             Post => ...

----------------------------------------
Objective: Combining Proof and Testing
----------------------------------------

* Some degree of testing is unavoidable

   - Social/legal reasons (Would you fly in an untested airplane?)
   - Limitations of formal proofs
   - Use of other, unverifiable languages along with SPARK

* Typically, infeasible to test every possible case

   - Huge number of cases, some unknown
   - Is deployment environment available? (e.g., Mars rovers)

* Hit theoretical limits at extreme reliability levels

   - *Butler, Ricky W.; Finelli, George B.* "The Infeasibility of Quantifying the Reliability of Life-critical Real-time Software"

-------------------------
Combined Proof and Test
-------------------------

* Combined proof and comprehensive testing is too expensive

   - Comprehensive testing is already too expensive

* Combined proof and selective testing can be less expensive than comprehensive testing alone

   - Only test things that cannot be proved (i.e., some unit tests)
   - Integration testing will remain vital

* Goal is to reach a level of confidence as good as the level reached by testing alone
* Industrial use

   - E.g., Airbus for avionics software, NVIDIA for automotive software, etc.

========
Lab
========

.. include:: labs/020_formal_methods_and_spark.lab.rst

=========
Summary
=========

----------------------------------
Formal Methods and SPARK Summary
----------------------------------

* Development of large, complex software is difficult

   - Especially so for high-integrity software

* SPARK does not abolish complexity or difficulty

   - But it does make it possible to reason about them!

* SPARK tools can prove extremely valuable properties
* No silver bullets, but far better costs and quality are possible

   - Demonstrated by real projects
