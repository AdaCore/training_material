=================
Type Invariants
=================

-------------------------------
Complicated Type Restrictions
-------------------------------

* Pre/postconditions add restrictions to subprogram calls/behavior

  * What about types (and therefore objects)?

* Sometimes low-level facilities can express it

  .. code:: Ada

    type Days is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
    subtype Weekdays is Days range Mon .. Fri;
    -- Restricts "Weekdays" to only part of "Days"

    type Bit_Array_T is array (Integer range <>) of Bit;
    type Flags_T is new Bit_Array_T (1 .. 8);
    -- Restricts "Flags_T" to always be 8 elements

* What about more complicated requirements?

  .. code:: Ada

    type Course_Description is record
       Start_Time : Ada.Calendar.Time;
       End_Time   : Ada.Calendar.Time;
    end record;
    --  How do we enforce "End_Time" > "Start_Time"?

    type Account is record
      Balance     : Currency;
      Deposits    : Currency_List;
      Withdrawals : Currency_List;
    end record;
    --  How do we ensure Balance is always accurate?
    
-----------------
Type Invariants
-----------------

* In Ada, a :dfn:`Type Invariant` is a condition that *always holds* for the client

  * But does not have to hold for the supplier

* Therefore it's only useful (and allowed for) private types

  .. code:: Ada

    package Bank is
      type Account is private with
        Type_Invariant => Consistent_Balance (Account);
      ...
    private
      type Account is record
        Balance     : Currency;
        Deposits    : Currency_List;
        Withdrawals : Currency_List;
      end record;

* But it makes more sense to "hide" the invariant completely

  .. code:: Ada

    package Bank is
      type Account is private;
      ...
    private
      type Account is record
        Balance     : Currency;
        Deposits    : Currency_List;
        Withdrawals : Currency_List;
      end record
        with Type_Invariant => Consistent_Balance (Account);

------------------------------
Type Invariant Verifications
------------------------------

* Automatically inserted by compiler
* Evaluated as postcondition of creation, evaluation, or return object

   - When objects first created
   - Assignment by clients
   - Type conversions

      * Creates new instances

* Not evaluated on internal state changes

   - Internal routine calls
   - Internal assignments

* Remember - these are abstract data types

.. image:: black_box_flow.svg

----------------------------------------
Invariant Over Object Lifetime (Calls)
----------------------------------------

.. image:: type_invariant_check_flow.svg

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

--------------------------------------------
Default Type Initialization for Invariants
--------------------------------------------

* Invariant must hold for initial value
* May need default type initialization to satisfy requirement

.. code:: Ada

   package Operations is
     -- Type is private, so we can't use Default_Value here
     type Private_T is private with Type_Invariant => Zero (Private_T);
     procedure Op (This : in out Private_T);
     function Zero (This : Private_T) return Boolean;
   private
     -- Type is not a record, so we need to use aspect
     -- (A record could use default values for its components)
     type Private_T is new Integer with Default_Value => 0;
     function Zero (This : Private_T) return Boolean is
     begin
        return (This = 0);
     end Zero;
   end Operations;

------------------------------
Invariants Are Not Foolproof
------------------------------

* Local subprograms are not checked

  * They could leave a parameter in an invalid state
  * Validity is not checked until public interface

* Elements that are access types provide back channel access

  * Pointed-to data could be modified outside the interface

* Exceptions may be propagated out of supplier

  * Can leave the parameter in an incomplete state

------
Quiz
------

.. container:: columns

 .. container:: column

  .. container:: latex_environment tiny

    .. code:: Ada

       package Counter is
          type Count_T is private;
          procedure Increment (Val : in out Count_T);
       private
          function Check_Limit (Value : Integer) 
                                return Boolean;
          type Count_T is new Integer with
             Type_Invariant =>
                Check_Limit (Integer (Count_T));
       end Counter;

       package body Counter is
          function Increment_Helper
            (Helper_Val : Count_T)
             return Count_T is
             Next_Value : Count_T := Helper_Val + 1;
          begin
             return Next_Value;
          end Increment_Helper;
          procedure Increment (Val : in out Count_T) is
          begin
             Val := Val + 1;
             Val := Increment_Helper (Val);
          end Increment;
          function Check_Limit (Value : Integer)
                                return Boolean is
             (Value <= 100); --  check against constraint
       end Counter;

 .. container:: column

    If `Increment` is called from outside of Counter, how many times is `Check_Limit` called?

       A. 1
       B. :answer:`2`
       C. 3
       D. 4

    .. container:: animate

       Type Invariants are only evaluated on entry into/exit from
       externally visible subprograms. So :ada:`Check_Limit` is called when
       entering/exiting :ada:`Increment` - not :ada:`Increment_Helper`

.. raw:: latex

  \vspace{5mm}
