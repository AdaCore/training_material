===================================
Preconditions and Postconditions
===================================

-----------------------------
Subprogram-based Assertions
-----------------------------

* **Explicit** part of a subprogram's **specification**

    - Unlike defensive code

* :dfn:`Precondition`

   - Assertion expected to hold **prior to** subprogram call

* :dfn:`Postcondition`

   - Assertion expected to hold **after** subprogram return

* Requirements and guarantees on both supplier and client
* Syntax uses **aspects**

   .. code:: Ada

      procedure Push (This : in out Stack_T;
                      Value : Content_T)
        with Pre  => not Full (This),
             Post => not Empty (This)
                     and Top (This) = Value;

---------------------------------
Requirements / Guarantees: Quiz
---------------------------------

* Given the following piece of code

    .. code:: Ada

       procedure Start is
       begin
           ...
           Turn_On;
           ...

       procedure Turn_On
        with Pre => Has_Power,
             Post => Is_On;

* Complete the table in terms of requirements and guarantees

.. list-table::

   * - 
     - Client (:ada:`Start`)
     - Supplier (:ada:`Turn_On`)

   * - Pre (:ada:`Has_Power`)
     - :animate:`Requirement`
     - :animate:`Guarantee`

   * - Post (:ada:`Is_On`)
     - :animate:`Guarantee`
     - :animate:`Requirement`

----------
Examples
----------

.. include:: ../examples/adv_270_subprogram_contracts/preconditions_and_postconditions.rst

---------------
Preconditions
---------------

* Define obligations on client for successful call

   - Precondition specifies required conditions
   - Clients must meet precondition for supplier to succeed

* Boolean expressions

   - Arbitrary complexity
   - Specified via aspect name :ada:`Pre`

* Checked prior to call by client

   - :ada:`Assertion_Error` raised if false

.. code:: Ada

   procedure Push (This : in out Stack;  Value : Content)
     with Pre => not Full (This);

----------------
Postconditions
----------------

* Define obligations on supplier

   - Specify guaranteed conditions after call

* Boolean expressions (same as preconditions)

   - Specified via aspect name :ada:`Post`

* Content as for preconditions, plus some extras
* Checked after corresponding subprogram call

   - :ada:`Assertion_Error` raised if false

.. code:: Ada

   procedure Push (This : in out Stack;  Value : Content)
     with Pre  => not Full (This),
          Post => not Empty (This) and Top (This) = Value;
   ...
   function Top (This : Stack) return Content
     with Pre => not Empty (This);

-------------------------------------
Postcondition :ada:`'Old` Attribute
-------------------------------------

* Values as they were just before the call
* Uses language-defined attribute :ada:`'Old`

   - Can be applied to most any visible object

      + :ada:`limited` types are forbidden
      + May be expensive

   - Expression can be **arbitrary**

        + Typically :ada:`out`, :ada:`in out` parameters and globals

   .. code:: Ada

      procedure Increment (This : in out Integer) with
          Pre  => This < Integer'Last,
          Post => This = This'Old + 1;

-------------------------------------------------
Function Postcondition :ada:`'Result` Attribute
-------------------------------------------------

* :ada:`function` result can be manipulated with :ada:`'Result`

   .. code:: Ada
      function Greatest_Common_Denominator (A, B : Integer)
        return Integer with
          Pre  =>  A > 0 and B > 0,
          Post =>  Is_GCD (A, B,
                           Greatest_Common_Denominator'Result);

------------------------------------------
Preconditions and Postconditions Example
------------------------------------------

* Multiple aspects separated by commas

.. code:: Ada

     procedure Push (This : in out Stack;
                     Value : Content)
       with Pre  => not Full (This),
            Post => not Empty (This) and Top (This) = Value;

------
Quiz
------

.. code:: Ada

   function Area (L : Positive; H : Positive) return Positive is
      (L * H)
   with Pre => ?

Which pre-condition is necessary for :ada:`Area` to calculate the correct result for
all values :ada:`L` and :ada:`H`?

   A. ``L > 0 and H > 0``
   B. ``L < Positive'Last and H < Positive'Last``
   C. ``L * H in Positive``
   D. :answer:`None of the above`

.. container:: animate

   Explanations

   A. Parameters are :ada:`Positive`, so this is unnecessary
   B. Overflow for large numbers
   C. Classic trap: the check itself may cause an overflow!

   The correct precondition would be

         :ada:`Integer'Last / L <= H`

   to prevent overflow errors on the range check.

------
Quiz
------

.. code:: Ada

   type Index_T is range 1 .. 100;
   -- Database initialized such that value for component at I = I
   Database : array (Index_T) of Integer;
   -- Set the value for component Index to Value and
   -- then increment Index by 1
   function Set_And_Move (Value :        Integer;
                          Index : in out Index_T)
                          return Boolean
      with Post => ...

Given the following expressions, what is their value if they are evaluated in the postcondition
of the call :ada:`Set_And_Move (-1, 10)`

.. list-table::

   * - ``Database'Old (Index)``

     - :animate:`11`
     - :animate:`Use new index in copy of original Database`

   * - ``Database (Index`Old)``

     - :animate:`-1`
     - :animate:`Use copy of original index in current Database`

   * - ``Database (Index)'Old``

     - :animate:`10`
     - :animate:`Evaluation of Database (Index) before call`

------------------------
Separations of Concerns
------------------------

* :ada:`Pre` and :ada:`Post` fit together

.. code:: Ada

   function Val return Integer
    with Post => F'Result /= 0
   is (if Val_Raw > 0 then Val_Raw else 1);

   procedure Process (I : Integer)
    with Pre => I /= 0
   is (Set_Output (10 / I));

   [...]

   Process (Val);

* Review of interface: guaranteed to work

    - What is returned by :ada:`Val` is always valid for :ada:`Process` 
    - Need to check implementations

* Review of implementation

    - :ada:`Val` **always** returns a value that is :ada:`/= 0`
    - :ada:`Process` accepts **any** value that is :ada:`/= 0`

* Great separation of concerns

    - a team (Clients) could be in charge of reviewing the interface part
    - another team (Suppliers) could be in charge of reviewing the implementation part
    - both would use the contracts as a common understanding
    - Tools can do an automated review / validation: :toolname:`GNAT Static Analysis Suite`, :toolname:`SPARK`

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
        Pre  => Open (This) and Funds_Available (This, Amount),
        Post => Balance (This) = Balance (This)'Old - Amount;
      ...
      function Funds_Available (This   : Account;
                                Amount : Currency)
                                return Boolean is
          (Amount > 0.0 and then Balance (This) >= Amount)
        with Pre => Open (This);

* A :ada:`function` may be unavoidable

   - Referencing private type components

------------------
Assertion Policy
------------------

* Assertions checks can be controlled with :ada:`pragma Assertion_Policy`

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

