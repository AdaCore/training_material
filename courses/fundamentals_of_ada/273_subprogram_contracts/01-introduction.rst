==============
Introduction
==============

--------------------------
:dfn:`Design-By-Contract`
--------------------------

* Source code acting in roles of **client** and **supplier** under a binding **contract**

   - :dfn:`Contract` specifies *requirements* or *guarantees*

      - *"A specification of a software element that affects its use by potential clients."* (Bertrand Meyer)

   - :dfn:`Supplier` provides services

       - Guarantees specific functional behavior
       - Has requirements for guarantees to hold

   - :dfn:`Client` utilizes services

       - Guarantees supplier's conditions are met
       - Requires result to follow the subprogram's guarantees

---------------
Ada Contracts
---------------

* Ada contracts include enforcement

   - At compile-time: specific constructs, features, and rules
   - At run-time: language-defined and user-defined exceptions

* Facilities as part of the language definition

   - Range specifications
   - Parameter modes
   - Generic contracts
   - OOP :ada:`interface` types
   - Work well, but on a restricted set of use-cases

* Contract aspects to be more expressive

   - Carried by subprograms
   - ... or by types (seen later)
   - Can have **arbitrary** conditions, more **versatile**

------------------
:dfn:`Assertion`
------------------

* Boolean expression expected to be :ada:`True`
* Said *to hold* when :ada:`True`
* Language-defined :ada:`pragma`

.. code:: Ada

   pragma Assert (not Full (Stack));
   -- stack is not full
   pragma Assert (Stack_Length = 0,
                  Message => "stack was not empty");
   -- stack is empty

* Raises language-defined :ada:`Assertion_Error` exception if expression does not hold
* The :ada:`Ada.Assertions.Assert` subprogram wraps it

.. code:: Ada

   package Ada.Assertions is
     Assertion_Error : exception;
     procedure Assert (Check : in Boolean);
     procedure Assert (Check : in Boolean; Message : in String);
   end Ada.Assertions;

------
Quiz
------

Which of the following statements is (are) correct?

    A. Contract principles apply only to newer versions of the language
    B. :answer:`Contract should hold even for unique conditions and corner cases`
    C. Contract principles were first implemented in Ada
    D. You cannot be both supplier and client

.. container:: animate

    Explanations

    A. No, but design-by-contract **aspects** were fully integrated into Ada 2012
    B. Yes, special case should be included in the contract
    C. No, in eiffel, in 1986!
    D. No, in fact you are always **both**, even the :ada:`Main` has a caller!

------
Quiz
------

Which of the following statements is (are) correct?

    A. :answer:`Assertions can be used in declarations`
    B. Assertions can be used in expressions
    C. :answer:`Any corrective action should happen before contract checks`
    D. Assertions must be checked using :ada:`pragma Assert`

.. container:: animate

    Explanations

    A. Will be checked at elaboration
    B. No assertion expression, but :ada:`raise` expression exists
    C. Exceptions as flow-control adds complexity, prefer a proactive :ada:`if` to a (reactive) :ada:`exception` handler
    D. You can call :ada:`Ada.Assertions.Assert`, or even directly :ada:`raise Assertion_Error`

------
Quiz
------

Which of the following statements is (are) correct?

    A. :answer:`Defensive coding is a good practice`
    B. Contracts can replace all defensive code
    C. Contracts are executable constructs
    D. Having exhaustive contracts will prevent run-time errors

.. container:: animate

    Explanations

    A. Principles are sane, contracts extend those
    B. See previous slide example
    C. e.g. generic contracts are resolved at compile-time
    D. A failing contract **will cause** a run-time error, only extensive (dynamic / static) analysis of contracted code may provide confidence in the absence of runtime errors (AoRTE)

