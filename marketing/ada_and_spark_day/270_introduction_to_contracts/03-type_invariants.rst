=================
Type Invariants
=================

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
