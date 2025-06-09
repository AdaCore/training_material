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

.. note::

   The unit already proves correctly already - we want to make sure
   that after each modification, the unit still proves correctly

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

----------------------------------
Using Expression Functions (1/3)
----------------------------------

- Define an expression function :ada:`Value_Rec_Is_One` to express the
  condition in the postcondition of :ada:`Init_Rec`

   + :ada:`Init_Rec` is supposed to set the active field to 1
   + After modification, verify the unit still proves correctly

.. container:: animate 2-

   .. code:: Ada

      function Value_Rec_Is_One (R : Rec) return Boolean is
        (Value_Rec (R) = 1);


   - Use :ada:`Value_Rec_Is_One` in the postcondition of :ada:`Init_Rec`

.. container:: animate 3-

   .. code:: Ada

      procedure Init_Rec (R : out Rec)
        with Post => Value_Rec_Is_One (R);

----------------------------------
Using Expression Functions (2/3)
----------------------------------

- Keep the declaration of :ada:`Value_Rec_Is_One` in the spec file, but move
  the expression function to the body file

   + After modification, verify the unit still proves correctly

.. container:: animate 2-

   - In spec

      .. code:: Ada

         function Value_Rec_Is_One (R : Rec) return Boolean;

         procedure Init_Rec (R : out Rec)
           with Post => Value_Rec_Is_One (R);

   - In body

      .. code:: Ada

         function Value_Rec_Is_One (R : Rec) return Boolean is
           (Value_Rec (R) = 1);

         procedure Init_Rec (R : out Rec) is
         begin
            case R.Disc is
            ...

----------------------------------
Using Expression Functions (3/3)
----------------------------------

- Turn the expression function of :ada:`Value_Rec_Is_One` into a regular
  function body

.. container:: animate 2-

   .. code:: Ada

      function Value_Rec_Is_One (R : Rec) return Boolean is
      begin
         return Value_Rec (R) = 1;
      end Value_Rec_Is_One;

   **Does** *the unit still prove correctly?*

.. container:: animate 3-

   - No! We have lost the "free" postcondition of an expression function

   - Add a postcondition to the declaration of :ada:`Value_Rec_Is_One`

.. container:: animate 4-

   .. code:: Ada

      function Value_Rec_Is_One (R : Rec) return Boolean
        with Post =>
          Value_Rec_Is_One'Result = (Value_Rec (R) = 1);

   **Now** the unit should prove correctly

------------------------
If You Have Time (1/2)
------------------------

- Implement the expression function :ada:`Constant_Value`

   .. code:: Ada

      function Constant_Value
         (T : Table; Start, Stop : Index; Value : Integer)
          return Boolean

   + Such that for every index between :ada:`Start` and :ada:`Stop` (inclusive), the
     element at that index is :ada:`Value`

.. container:: animate 2-

   - Hint: Use a precondition to make sure input parameters make sense

.. container:: animate 3-

   .. code:: Ada

      function Constant_Value
        (T : Table; Start, Stop : Index; Value : Integer)
         return Boolean
      is
        (for all J in Start .. Stop => T (J) = Value)
      with
        Pre => Start > Stop or else (Start in T'Range and then Stop in T'Range);

   **Note:** *Zero length arrays are defined as* :ada:`'First` *being larger than* :ada:`'Last`.
   *So our precondition verifes that* :ada:`Start` *and* :ada:`Stop` *are valid indices*
   *into the array*

------------------------
If You Have Time (2/2)
------------------------

- Using :ada:`Constant_Value`, write a postcondition for :ada:`Init_Table` where

   + The first and last elements have the correct values of "1" and "2"
   + All other elements are set to "0"
   + Verify the unit still proves correctly

.. container:: animate 2-

   .. code:: Ada

      procedure Init_Table (T : out Table)
        with
          Pre  => T'Length >= 2,
          Post => T (T'First) = 1
                  and then T (T'Last) = 2
                  and then Constant_Value
                          (T     => T,
                           Start => T'First + 1,
                           Stop  => T'Last - 1,
                           Value => 0);
