=============
In Practice
=============

----------------------------------------
Pre/Postconditions: to Be or Not to Be
----------------------------------------

* **Preconditions** are reasonable **default** for run-time checks
* **Postconditions** advantages can be **comparatively** low

   - Use of :ada:`'Old` and :ada:`'Result` with (maybe deep) copy
   - Very useful in **static analysis** contexts (Hoare triplets)

* For **trusted** library, enabling **preconditions only** makes sense

   - Catch **user's errors**
   - Library is trusted, so :ada:`Post => True` is a reasonable expectation

* Typically contracts are used for **validation**
* Enabling subprogram contracts in production may be a valid trade-off depending on...

   - Exception failure **trace availability** in production
   - Overall **timing constraints** of the final application
   - Consequences of violations **propagation**
   - Time and space **cost** of the contracts

* Typically production settings favor telemetry and off-line analysis

-------------------------------------
No Secret Precondition Requirements
-------------------------------------

* Client should be able to **guarantee** them
* Enforced by the compiler

.. code:: Ada

   package Some_Package is
     function Foo return Bar
       with Pre => Hidden; -- illegal private reference
   private
     function Hidden return Boolean;
   end Some_Package;

---------------------------------------
Postconditions Are Good Documentation
---------------------------------------

.. code:: Ada

   procedure Reset
       (Unit : in out DMA_Controller;
        Stream : DMA_Stream_Selector)
     with Post =>
       not Enabled (Unit, Stream) and
       Operating_Mode (Unit, Stream) = Normal_Mode and
       Selected_Channel (Unit, Stream) = Channel_0 and
       not Double_Buffered (Unit, Stream) and
       Priority (Unit, Stream) = Priority_Low and
       (for all Interrupt in DMA_Interrupt =>
           not Interrupt_Enabled (Unit, Stream, Interrupt));

--------------------------------------
Postcondition Compared to Their Body
--------------------------------------

* Specifying relevant properties may "repeat" the body

   - Unlike preconditions
   - Typically **simpler** than the body
   - Closer to a **re-phrasing** than a tautology

* Good fit for *hard to solve and easy to check* problems

   - Solvers: :ada:`Solve (Find_Root'Result, Equation) = 0`
   - Search: :ada:`Can_Exit (Path_To_Exit'Result, Maze)`
   - Cryptography: :ada:`Match (Signer (Sign_Certificate'Result), Key.Public_Part)`

* Bad fit for poorly-defined or self-defining subprograms

.. code:: Ada

    function Get_Magic_Number return Integer
        with Post => Get_Magic_Number'Result = 42
        -- Useless post-condition, simply repeating the body
        is (42);

-----------------------------------------------
Postcondition Compared to Their Body: Example
-----------------------------------------------

.. code:: Ada

   function Greatest_Common_Denominator (Num1, Num2 : Natural)
     return Integer with
     Post =>  Is_GCD (Num1,
                      Num2,
                      Greatest_Common_Denominator'Result);

   function Is_GCD (Num1, Num2, Candidate : Integer)
       return Boolean is
     (Num1 rem Candidate = 0 and
      Num2 rem Candidate = 0 and
      (for all K in 1 .. Integer'Min (Num1,Num2) =>
         (if (Num1 rem K = 0 and Num2 rem K = 0)
          then K <= Candidate)));

----------------------
Contracts Code Reuse
----------------------

* Contracts are about **usage** and **behaviour**

   - Not optimization
   - Not implementation details
   - **Abstraction** level is typically high

* Extracting them to :ada:`function` is a good idea

   - *Code as documentation, executable specification*
   - Completes the **interface** that the client has access to
   - Allows for **code reuse**

   .. code:: Ada

      procedure Withdraw (This   : in out Account;
                          Amount :        Currency) with
        Pre  => Open (This) and then Funds_Available (This, Amount),
        Post => Balance (This) = Balance (This)'Old - Amount;
      ...
      function Funds_Available (This   : Account;
                                Amount : Currency)
                                return Boolean is
          (Amount > 0.0 and then Balance (This) >= Amount)
        with Pre => Open (This);

* A :ada:`function` may be unavoidable

   - Referencing private type components

---------------------------------------
Subprogram Contracts on Private Types
---------------------------------------

.. code:: Ada

   package Bank is
     type Account is private;
     procedure Process_Transaction (This : Account) with
       Pre => This.Balance > 0; -- not legal
     ...
     function Current_Balance (This : Account) return Integer;
     ...
     procedure R (This : Account) with
       Pre => Current_Balance (This) > 0; -- legal
     ...
   private
     type Account is record
       Balance : Natural;
       ...
     end record;
     function Current_Balance (This : Account) return Integer is
         (This.Balance);
   end Bank;

-----------------------------------
Preconditions or Explicit Checks?
-----------------------------------

* Any requirement from the spec should be a pre-condition

   - If clients need to know the body, abstraction is **broken**

* With pre-conditions

   .. code:: Ada

      type Stack (Capacity : Positive) is tagged private;
      procedure Push (This : in out Stack;
                      Value : Content) with
        Pre  => not Full (This);

* With defensive code, comments, and return values

   .. code:: Ada

      -- returns True iff push is successful
      function Try_Push (This : in out Stack;
                         Value : Content) return Boolean
      begin
        if Full (This) then
            return False;
        end if;
        ...

* But not both

   - For the implementation, preconditions are a **guarantee**
   - A subprogram body should **never** test them

-----------------------------
Raising Specific Exceptions
-----------------------------

* In the Exceptions module, we show how user-defined exceptions are better than pre-defined

   * Stack :ada:`Push` raising :ada:`Overflow_Error` rather than :ada:`Constraint_Error`

* *Default* behavior for a preconditon failure is :ada:`Assertion_Error`

   * But it doesn't have to be!

* Use *raise expression* in a precondition to get a different exception

   .. code:: Ada

      procedure Push (This : in out Stack;
                      Value : Content) with
        Pre  => not Full (This) or else raise Overflow_Error;

* *Note: Postcondition failure only ever makes sense as an Assertion_Error*

  * It's the supplier's fault, not the client's

------------------
Assertion Policy
------------------

* Pre/postconditions can be controlled with :ada:`pragma Assertion_Policy`

   .. code:: Ada
      
      pragma Assertion_Policy
           (Pre => Check,
            Post => Ignore);

* Fine **granularity** over assertion kinds and policy identifiers

   :url:`https://docs.adacore.com/gnat_rm-docs/html/gnat_rm/gnat_rm/implementation_defined_pragmas.html#pragma-assertion-policy`


* Certain advantage over explicit checks which are **harder** to disable

   - Conditional compilation via global :ada:`constant Boolean`

   .. code:: Ada

      procedure Push (This : in out Stack;  Value : Content) is
      begin
        if Debugging then
          if Full (This) then
            raise Overflow;
          end if;
        end if;

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
    B. Contracts prevent interface issues, not processing problems
    C. For example, generic contracts are resolved at compile-time
    D. A failing contract will **cause** a run-time error; only extensive (dynamic/static) analysis of contracted code may provide confidence in the absence of runtime errors (AoRTE)
