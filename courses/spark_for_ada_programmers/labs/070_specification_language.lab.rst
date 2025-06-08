=====
Lab
=====

----------------------------
Specification Language Lab
----------------------------

- Find the :filename:`070_specification_language` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

----------------------------------------
Demonstrating Richer Expressions (1/3)
----------------------------------------

- This part of the lab is showing how to use some newer language constructs in pre-/postconditions

   + The unit proves correctly already
   + After each modification, check that the code is still proved

- Use a *declare expression* to introduce names :ada:`X_Old` annd :ada:`Y'Old` in the postcondition of :ada:`Swap`

.. container:: animate 2-

   .. code:: Ada

      procedure Swap (X, Y : in out Integer)
        with Post =>
          (declare
             X_Old : constant Integer := X'Old;
             Y_Old : constant Integer := Y'Old;
           begin
             X = Y_Old and then Y = X_Old);

.. container:: animate 1-

   *Verify the unit still proves correctly*

----------------------------------------
Demonstrating Richer Expressions (2/3)
----------------------------------------

- Use *delta aggregates* to state the new value of :ada:`R` in the
  postcondition of :ada:`Bump_Rec`

.. container:: animate 2-

   + Hint: use an *if expression* testing the value of the discriminant

.. container:: animate 3-

   .. code:: Ada

      procedure Bump_Rec (R : in out Rec)
       with
         Pre  => Value_Rec (R) < Integer'Last,
         Post =>
           (if R.Disc then
              R = (R'Old with delta A => Value_Rec (R)'Old + 1)
            else
              R = (R'Old with delta B => Value_Rec (R)'Old + 1));

.. container:: animate 1-

   *Verify the unit still proves correctly*

----------------------------------------
Demonstrating Richer Expressions (3/3)
----------------------------------------

- Use a *quantified expression* to state that all values in array :ada:`T` are
  preserved after the call to :ada:`Swap_Table`

   + Except for those at indexes :ada:`I` and :ada:`J`

.. container:: animate 2-

   + Hint: use a membership test for "being different from :ada:`I` and :ada:`J`"
   + Hint: notice that :ada:`T'Old(K)` may be allowed even if :ada:`T(K)'Old` is not

.. container:: animate 3-

   .. code:: Ada

      procedure Swap_Table (T : in out Table; I, J : Index)
      with
        Pre  => I in T'Range and then J in T'Range,
        Post => T (I) = T (J)'Old and then T (J) = T (I)'Old
          and then (for all K in T'Range =>
                      (if K not in I | J then T (K) = T'Old (K)));

.. container:: animate 1-

   *Verify the unit still proves correctly*

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
