========================
Preventing Unsoundness
========================

--------------------
Quiz - Unsoundness
--------------------

What's wrong with the following contract?

.. code:: ada

   function Half (Value : Integer) return Integer
     with Post => Value = 2 * Half'Result;

.. container:: animate

   * The postcondition is false when :ada:`Value` is odd
   * :toolname:`GNATprove` generates an inconsistent axiom for :ada:`Half`

     - It says that any integer is equal to twice another integer
     - This can be used by provers to deduce :ada:`False`
     - **Anything** can be proved from :ada:`False`

       + As if the code was dead code

----------------------
Unfeasible Contracts
----------------------

* All contracts **should** be feasible

  - There exists a correct implementation
  - This includes absence of runtime errors

* Contract of :ada:`Double` also leads to **unsoundness**

  - The postcondition is false when :ada:`Value` is too large

  .. code:: ada

     function Double (Value : Integer) return Integer
       with Post => Double'Result = 2 * Value;

* :toolname:`GNATprove` implements defense in depth

  - Axiom only generated for functions (not procedures)
  - Function **sandboxing** adds a guard to the axiom

    + Unless switch :command:`--function-sandboxing=off` is used

  - Switch :command:`--proof-warnings=on` can detect inconsistencies
  - Proof of subprogram will detect contract unfeasibility

    + **Except** when subprogram does not terminate

---------------------------
Non-terminating Functions
---------------------------

What's wrong with the following code?

.. code:: ada

   function Half (Value : Integer) return Integer is
   begin
      if True then
         return Half (Value);
      else
         return 0;
      end if;
   end Half;

.. container:: animate

   * Function :ada:`Half` does not terminate
   * :toolname:`GNATprove` proves the postcondition of :ada:`Half`!

     - Because that program point is unreachable (dead code)

   * :toolname:`GNATprove` does not generate an axiom for :ada:`Half`

     - Because function may not terminate
     - :command:`info: function contract not available for proof`
     - Info message issued when using switch :command:`--info`

-----------------------
Terminating Functions
-----------------------

* Functions should **always** terminate

* Specific contract to require proof of termination of procedures

  .. code:: ada

     procedure P
       with Always_Terminates => Condition;

* Flow analysis proves termination in **simple cases**

  - No (mutually) recursive calls
  - Only bounded loops

* **Proof** used to prove termination in remaining cases

  - Based on subprogram variant for recursive subprograms
  - Based on loop variant for unbounded loops

---------------------
Subprogram Variants
---------------------

* Specifies measure on recursive calls

  - Either increases or decreases strictly

.. code:: ada

   function Half (Value : Integer) return Integer
     Subprogram_Variant =>
       (Increases => (if Value > 0 then -Value else Value)),
   is
   begin
      if Value in -1 .. 1 then
         return 0;
      elsif Value > 1 then
         return 1 + Half (Value - 2);
      else
         return -1 + Half (Value + 2);
      end if;
   end Half;

* More complex cases use lexicographic order

.. code:: ada

   Subprogram_Variant => (Decreases => Integer'Max(Value, 0),
                          Increases => Integer'Min(Value, 0)),

