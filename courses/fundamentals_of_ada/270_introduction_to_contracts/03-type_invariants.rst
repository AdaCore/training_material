================
Type Invariants
================

---------------
Strong Typing
---------------

* Ada supports strong typing

   .. code:: Ada

      type Small_Integer_T is range -1_000 .. 1_000;
      type Enumerated_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
      type Array_T is array (1 .. 3) of Boolean;

* What if we need stronger enforcement?

   * Number must be even
   * Subset of non-consecutive enumerals
   * Array should always be sorted

* **Type Invariant**

   * Property of type that is always true on **external** reference
   * *Guarantee* to client, similar to subprogram postcondition

* **Subtype Predicate**

   * Property of type that is always true, unconditionally
   * Can add arbitrary constraints to a type, unlike the "basic" type system

----------
Examples
----------

.. include:: ../examples/adv_275_type_contracts/type_invariants.rst

----------------
Type Invariant
----------------

* Applied to :ada:`private` types
* Evaluated as postcondition of creation, evaluation, or return object

   - When objects first created
   - Assignment by clients
   - Type conversions

      * Creates new instances

* Not evaluated on internal state changes

   - Internal routine calls
   - Internal assignments

* Remember - these are abstract data types

.. image:: black_box_flow.png

----------------------------------------
Invariant Over Object Lifetime (Calls)
----------------------------------------

.. image:: type_invariant_check_flow.png

.. container:: speakernote

   Note that other actions also invoke the checks!

------------------------
Example Type Invariant
------------------------

* A bank account balance must always be consistent

   - Consistent Balance:  Total Deposits  - Total Withdrawals  =  Balance

.. code:: Ada

   package Bank is
     type Account is private with
       Type_Invariant => Consistent_Balance (Account);
     ...
     -- Called automatically for all Account objects
     function Consistent_Balance (This : Account)
       return Boolean;
     ...
   private
     ...
   end Bank;

-----------------------------------
Invariants Don't Apply Internally
-----------------------------------

* No checking within supplier package

   - Otherwise there would be no way to implement anything!

* Only matters when clients can observe state

.. code:: Ada

   procedure Open (This : in out Account;
                   Name : in String;
                   Initial_Deposit : in Currency) is
   begin
     This.Owner := To_Unbounded_String (Name);
     This.Current_Balance := Initial_Deposit;
     -- invariant would be false here!
     This.Withdrawals := Transactions.Empty_Vector;
     This.Deposits := Transactions.Empty_Vector;
     This.Deposits.Append (Initial_Deposit);
     -- invariant is now true
   end Open;

------
Quiz
------

.. container:: columns

 .. container:: column

  .. container:: latex_environment tiny

   .. code:: Ada

      package P is
         type Some_T is private;
         procedure Do_Something (X : in out Some_T);
      private
         function Counter (I : Integer) return Boolean;
         type Some_T is new Integer with
            Type_Invariant => Counter (Integer (Some_T));
      end P;

      package body P is
         function Local_Do_Something (X : Some_T)
                                      return Some_T is
            Z : Some_T := X + 1;
         begin
            return Z;
         end Local_Do_Something;
         procedure Do_Something (X : in out Some_T) is
         begin
            X := X + 1;
            X := Local_Do_Something (X);
         end Do_Something;
         function Counter (I : Integer)
                           return Boolean is
            (True);
      end P;

 .. container:: column

    If `Do_Something` is called from outside of P, how many times is `Counter` called?

       A. 1
       B. :answer:`2`
       C. 3
       D. 4

    .. container:: animate

       Type Invariants are only evaluated on entry into and exit from
       externally visible subprograms. So :ada:`Counter` is called when
       entering and exiting :ada:`Do_Something` - not :ada:`Local_Do_Something`,
       even though a new instance of :ada:`Some_T` is created

