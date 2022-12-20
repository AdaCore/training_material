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

  + After each modification, check that the code is still proved by :code:`GNATprove`

- Use a *declare expression* to introduce names :code:`X_Old` for :code:`X'Old`
  and :code:`Y_Old` for :code:`Y'Old` in the postcondition of :code:`Swap`

- Use *delta aggregates* to state the new value of :code:`R` in the
  postcondition of :code:`Bump_Rec`

  + Hint: use an *if expression* testing the value of the discriminant

- Use a *quantified expression* to state that all values in array :code:`T` are
  preserved after the call to :code:`Swap_Table`, except for those at indexes
  :code:`I` and :code:`J`

  + Hint: use a membership test for "being different from :code:`I` and :code:`J`"
  + Hint: notice that :code:`T'Old(K)` may be allowed even if :code:`T(K)'Old` is not

----------------------
Expression Functions
----------------------

- Define an expression function :code:`Value_Rec_Is_One` to express the
  condition in the postcondition of :code:`Init_Rec`

- Use :code:`Value_Rec_Is_One` in the postcondition of :code:`Init_Rec`

  + Check that the code is still proved

- Keep the declaration of :code:`Value_Rec_Is_One` in the spec file, but move
  the expression function in the body file.

  + Is the code still proved?

- Turn the expression function of :code:`Value_Rec_Is_One` into a regular
  function body.

  + Is the code still proved?

- Add a postcondition to the declaration of :code:`Value_Rec_Is_One` into a regular
  function body.

  + Is the code proved again?

- Discuss these with the course instructor.

--------------
All Together
--------------

- Define a function :code:`Constant_Value` that returns :code:`True` if an
  array :code:`T` has value :code:`Value` between indexes :code:`Start` and
  :code:`Stop`

  + Hint: add a precondition to exclude incorrect parameter values

- Use :code:`Constant_Value` in the postcondition of :code:`Init_Table` to
  express that the table has value zero at all indexes except the first and
  last ones.

- Check that the code is still proved.
