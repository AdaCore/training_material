----------------------------
Specification Language Lab
----------------------------

- Find the :filename:`7_specification_language` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

--------------------
Richer Expressions
--------------------

- Find and open the files :filename:`basics.ads` and :filename:`basics.adb` in :toolname:`GNAT Studio`

  + After each modification, check that the code is still proved by :tooname:`GNATprove`

- Use a *declare expression* to introduce names :ada:`X_Old` for :ada:`X'Old`
  and :ada:`Y_Old` for :ada:`Y'Old` in the postcondition of :ada:`Swap`

- Use *delta aggregates* to state the new value of :ada:`R` in the
  postcondition of :ada:`Bump_Rec`

  + Hint: use an *if expression* testing the value of the discriminant

- Use a *quantified expression* to state that all values in array :ada:`T` are
  preserved after the call to :ada:`Swap_Table`, except for those at indexes
  :ada:`I` and :ada:`J`

  + Hint: use a membership test for "being different from :ada:`I` and :ada:`J`"
  + Hint: notice that :ada:`T'Old(K)` may be allowed even if :ada:`T(K)'Old` is not

----------------------
Expression Functions
----------------------

- Define an expression function :ada:`Value_Rec_Is_One` to express the
  condition in the postcondition of :ada:`Init_Rec`

- Use :ada:`Value_Rec_Is_One` in the postcondition of :ada:`Init_Rec`

  + Check that the code is still proved

- Keep the declaration of :ada:`Value_Rec_Is_One` in the spec file, but move
  the expression function in the body file.

  + Is the code still proved?

- Turn the expression function of :ada:`Value_Rec_Is_One` into a regular
  function body.

  + Is the code still proved?

- Add a postcondition to the declaration of :ada:`Value_Rec_Is_One` into a regular
  function body.

  + Is the code proved again?

- Discuss these with the course instructor.

--------------
All Together
--------------

- Define a function :ada:`Constant_Value` that returns :ada:`True` if an
  array :ada:`T` has value :ada:`Value` between indexes :ada:`Start` and
  :ada:`Stop`

  + Hint: add a precondition to exclude incorrect parameter values

- Use :ada:`Constant_Value` in the postcondition of :ada:`Init_Table` to
  express that the table has value zero at all indexes except the first and
  last ones.

- Check that the code is still proved.
