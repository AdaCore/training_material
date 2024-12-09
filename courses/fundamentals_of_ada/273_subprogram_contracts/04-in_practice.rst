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

   package P is
     function Foo return Bar
       with Pre => Hidden; -- illegal private reference
   private
     function Hidden return Boolean;
   end P;

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

   function Greatest_Common_Denominator (A, B : Natural)
     return Integer with
     Post =>  Is_GCD (A,
                      B,
                      Greatest_Common_Denominator'Result);

   function Is_GCD (A, B, Candidate : Integer)
       return Boolean is
     (A rem Candidate = 0 and
      B rem Candidate = 0 and
      (for all K in 1 .. Integer'Min (A,B) =>
         (if (A rem K = 0 and B rem K = 0)
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

   package P is
     type T is private;
     procedure Q (This : T) with
       Pre => This.Total > 0; -- not legal
     ...
     function Current_Total (This : T) return Integer;
     ...
     procedure R (This : T) with
       Pre => Current_Total (This) > 0; -- legal
     ...
   private
     type T is record
       Total : Natural ;
       ...
     end record;
     function Current_Total (This : T) return Integer is
         (This.Total);
   end P;

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
        Pre  => not Full (This) or else Overflow_Error;

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

