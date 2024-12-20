=================
Type Invariants
=================

-----------------
Type Invariants
-----------------

* There may be conditions that must hold over entire lifetime of objects

   - Pre/postconditions apply only to subprogram calls

* Sometimes low-level facilities can express it

   .. code:: Ada

      subtype Weekdays is Days range Mon .. Fri;

      -- Guaranteed (absent unchecked conversion)
      Workday : Weekdays := Mon;

* Type invariants apply across entire lifetime for complex abstract data types
* Part of ADT concept, so only for private types

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

-------------------------------------------
Example Type Invariant Implementation
-------------------------------------------

.. code:: Ada

   package body Bank is
   ...
     function Total (This : Transaction_Vector)
         return Currency is
       Result : Currency := 0.0;
     begin
       for Value of This loop
         Result := Result + Value;
       end loop;
       return Result;
     end Total;
     function Consistent_Balance (This : Account)
         return Boolean is
     begin
       return Total (This.Deposits) - Total (This.Withdrawals)
              = This.Current_Balance;
     end Consistent_Balance;
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

---------------------------------
Type Invariant Clause Placement
---------------------------------

* Can move aspect clause to private part

   .. code:: Ada

      package Operations is
        type Private_T is private;
        procedure Op (This : in out Private_T);
      private
        type Private_T is new Integer with
          Type_Invariant => Private_T = 0,
          Default_Value => 0;
      end Operations;

* It is really an implementation aspect

   * Client shouldn't care!

.. container:: speakernote

   Alternatively, declaring the 'Zero' predicate function and making it visible to clients will allow them to re-state the invariant for subclasses.
   That's useful because new, added primitive operations do not inherit the parent's type invariant.
   In other words the invariant isn't really inherited, it just comes for free with those primitives that are inherited (and not overridden).

------------------------------
Invariants Are Not Foolproof
------------------------------

* Access to ADT representation via pointer could allow back door manipulation
* These are private types, so access to internals must be granted by the private type's code
* Granting internal representation access for an ADT is a highly questionable design!

------
Quiz
------

.. container:: columns

 .. container:: column

  .. container:: latex_environment tiny

   .. code:: Ada

      package Counter_Package is
         type Counter_T is private;
         procedure Increment (C : in out Counter_T);
      private
         function Check_Threshold (Value : Integer) 
                                       return Boolean;
         type Counter_T is new Integer with
            Type_Invariant => Check_Threshold 
                              (Integer (Counter_T));
      end Counter_Package;

      package body Counter_Package is
         function Increment_Helper (C : Counter_T)
                                      return Counter_T is
            Next_Value : Counter_T := C + 1;
         begin
            return Next_Value;
         end Local_Do_Something;
         procedure Increment (C : in out Counter_T) is
         begin
            C := C + 1;
            C := Increment_Helper (C);
         end Increment;
         function Check_Threshold (Value : Integer)
                                          return Boolean is
            (Value <= 100); --  check against constraint
      end Counter_Package;

 .. container:: column

    If `Increment` is called from outside of Counter_Package, how many times is `Check_Threshold` called?

       A. 1
       B. :answer:`2`
       C. 3
       D. 4

    .. container:: animate

       Type Invariants are only evaluated on entry into/exit from
       externally visible subprograms. So :ada:`Check_Threshold` is called when
       entering/exiting :ada:`Increment` - not :ada:`Increment_Helper`
