--------------------
Type Contracts Lab
--------------------

- Find the :filename:`9_type_contracts` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

-----------------
Type Predicates
-----------------

- Find and open the files :filename:`basics.ads` and :filename:`basics.adb` in :toolname:`GNAT Studio`

- Run :toolname:`GNATprove` to prove the unit

  + Look at unproved predicate checks, can you explain them?
  + Does it make a difference that :code:`Swap_Pair` is public and
    :code:`Bump_Pair` is private?

- Fix the predicate check failure in :code:`Bump_Pair`

  + Hint: use an aggregate assignment

- Fix the predicate check failure in :code:`Swap_Pair` by using a base type
  without predicate for :code:`Pair`

-----------------
Type Invariants
-----------------

- Run :toolname:`GNATprove` to prove the unit

  + Look at unproved invariant checks, can you explain them?
  + Does it make a difference that :code:`Swap_Triplet` is public and
    :code:`Bump_Triplet` is private?

- Fix the invariant check failure on the default value for :code:`Triplet`

- Fix the invariant check failure in :code:`Swap_Triplet`

  + Hint: the intent is for the value of all components to rotate

- Fix the invariant check failure in :code:`Bump_And_Swap_Triplet`

  + Hint: look also at :code:`Bump_Triplet`
  + Hint: you will need to add a postcondition to :code:`Bump_Triplet`

--------------
All Together
--------------

- Run :toolname:`GNATprove` to prove the unit and display all proved checks

- Can you explain the presence of predicate checks and invariant checks?

  + How about the absence of checks in :code:`Bump_And_Swap_Pair`?
  + How about the checks in :code:`Bump_And_Swap_Triplet`?
