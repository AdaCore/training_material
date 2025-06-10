=====
Lab
=====

--------------------
Type Contracts Lab
--------------------

- Find the :filename:`090_type_contracts` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

-----------------------
Type Predicates (1/2)
-----------------------

.. container:: animate 1-

   - Run :toolname:`GNATprove` to prove the unit

.. container:: animate 2-

   From inside :filename:`basic.ads` right-click and select
   :menu:`SPARK` |rightarrow| :menu:`Prove File`

.. container:: animate 1-

   - Can you understand the messages?
   - How would you "help" the prover?

.. container:: animate 2-

   :color-red:`basics.adb:5:8: medium: predicate check might fail`

   :color-red:`basics.adb:12:8: medium: predicate check might fail`

   :color-red:`basics.ads:10:1: possible fix: subprogram at basics.ads:10 should mention P in a precondition`

   :color-red:`basics.adb:38:8: medium: invariant check might fail`

   :color-red:`basics.ads:19:1: medium: for T before the call at basics.ads:19`

   :color-red:`basics.ads:19:14: medium: invariant check might fail`

   :color-red:`basics.ads:19:1: medium: for T at the end of Swap_Triplet at basics.ads:19`

   :color-red:`basics.ads:39:9: medium: invariant check might fail on default value`

-----------------------
Type Predicates (2/2)
-----------------------

.. container:: animate 1-

   - Fix the predicate check failure in :ada:`Bump_Pair`

.. container:: animate 2-

   *Hint: use an aggregate assignment*

.. container:: animate 3-

   .. code:: Ada

      procedure Bump_Pair (P : in out Pair) is
      begin
         P := Pair'(X => P.X + 1, Y => P.Y + 1);
      end Bump_Pair;

.. container:: animate 1-

   - Fix the predicate check failure in :ada:`Swap_Pair` by making :ada:`Pair`
     a subtype of a type without a predicate

.. container:: animate 4-

   - Update the spec

      .. code:: Ada

         type Base_Pair is record
            X, Y : Integer;
         end record;

         subtype Pair is Base_Pair
           with Predicate => Pair.X /= Pair.Y;

   - Update the body

      .. code:: Ada

         procedure Swap_Pair (P : in out Pair) is
            Base : Base_Pair := P;
            Tmp  : Integer := P.X;
         begin
            Base.X := Base.Y;
            Base.Y := Tmp;
            P := Base;
         end Swap_Pair;

-----------------
Type Invariants
-----------------

- Run :toolname:`GNATprove` to prove the unit

   + Look at unproved invariant checks, can you explain them?
   + Does it make a difference that :ada:`Swap_Triplet` is public and
     :ada:`Bump_Triplet` is private?

- Fix the invariant check failure on the default value for :ada:`Triplet`

- Fix the invariant check failure in :ada:`Swap_Triplet`

   + Hint: the intent is for the value of all components to rotate

- Fix the invariant check failure in :ada:`Bump_And_Swap_Triplet`

   + Hint: look also at :ada:`Bump_Triplet`
   + Hint: you will need to add a postcondition to :ada:`Bump_Triplet`

--------------
All Together
--------------

- Run :toolname:`GNATprove` to prove the unit and display all proved checks

- Can you explain the presence of predicate checks and invariant checks?

   + How about the absence of checks in :ada:`Bump_And_Swap_Pair`?
   + How about the checks in :ada:`Bump_And_Swap_Triplet`?
