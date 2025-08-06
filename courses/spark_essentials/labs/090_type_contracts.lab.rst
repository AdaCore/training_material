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

   - Look at the **predicate check** messages
   - How would you "help" the prover?

.. container:: animate 2-

   :color-red:`basics.adb:5:8: medium: predicate check might fail`

   :color-red:`basics.adb:12:8: medium: predicate check might fail`

   :color-red:`basics.ads:10:1: possible fix: subprogram at basics.ads:10 should mention P in a precondition`

   *(Ignore remaining messages for now)*

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

-----------------------
Type Invariants (1/4)
-----------------------

.. container:: animate 1-

   - Run :toolname:`GNATprove` to prove the unit

      - Predicate check messages should be gone

   - Look at the **invariant check** messages
   - How would you "help" the prover?

.. container:: animate 2-

   :color-red:`basics.adb:39:8: medium: invariant check might fail`

   :color-red:`basics.ads:21:1: medium: for T before the call at basics.ads:21`

   :color-red:`basics.ads:21:14: medium: invariant check might fail`

   :color-red:`basics.ads:21:1: medium: for T at the end of Swap_Triplet at basics.ads:21`

   :color-red:`basics.ads:41:9: medium: invariant check might fail on default value`

-----------------------
Type Invariants (2/4)
-----------------------

- Fix the invariant check failure on the default value for :ada:`Triplet`

.. container:: animate 2-

   *Hint: Need to ensure default value satisfies the invariant*

.. container:: animate 3-

   .. code:: Ada

      type Triplet is record
         A : Integer := 0;
         B : Integer := 1;
         C : Integer := 2;
      end record
        with Invariant => All_Different (Triplet);

-----------------------
Type Invariants (3/4)
-----------------------

- Fix the invariant check failure in :ada:`Swap_Triplet`

.. container:: animate 2-

   *Hint: the intent is for the value of all components to rotate*

.. container:: animate 3-

   .. code:: Ada

      procedure Swap_Triplet (T : in out Triplet) is
      begin
         T := (A => T.B, B => T.C, C => T.A);
      end Swap_Triplet;

-----------------------
Type Invariants (4/4)
-----------------------

- Fix the invariant check failure in :ada:`Bump_And_Swap_Triplet`

.. container:: animate 2-

   + Hint: look also at :ada:`Bump_Triplet` - the prover needs to
     know the result of that call
   
.. container:: animate 3-

   .. code:: Ada

      procedure Bump_Triplet (T : in out Triplet)
      with
        Pre  => T.A < Integer'Last and
                T.B < Integer'Last and
                T.C < Integer'Last,
        Post => T.A = T.A'Old + 1 and
                T.B = T.B'Old + 1 and
                T.C = T.C'Old + 1;

   * But this isn't enough! We know what is *supposed* to happen, but
     it isn't what actually happens!

     * The prover has found a bug!
     * Fix the code for :ada:`Bump_Triplet`
