=====
Lab
=====

-----------------------
Auto-active Proof Lab
-----------------------

- Find the :filename:`130_autoactive_proof` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place
   + Open a command prompt in that directory

- Windows: From the command line, run the :filename:`gpr_project_path.bat` file to set up your project path

   + The file resides in the :filename:`source` folder you installed
   + Pass in the version of SPARK you have installed (e.g. :command:`gpr_project_path 25.1`)
   + This only needs to be done once per command prompt window

.. note::

   For Linux users, the install location for SPARK varies greatly, so instead there is
   a shell script :filename:`gpr_project_path.sh` which gives you directions

- From the command-line, run :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

----------------
Selection Sort
----------------

- Find and open the files :filename:`sort_types.ads`, :filename:`sort.ads` and :filename:`sort.adb` in :toolname:`GNAT Studio`

- Examine the code - especially the comments!

   - Understand how the utility functions :ada:`Swap` and :ada:`Index_Of_Minimum`
     are used to perform the sort

   - Understand how the helper functions :ada:`Is_Permutation_Array`, :ada:`Is_Perm`,
     and :ada:`Is_Sorted` will help prove :ada:`Selection_Sort`

-----------------------
Proving the Utilities 
-----------------------

.. container:: animate 1-

   - Add a full functional contract to procedure :ada:`Swap` and prove it

.. container:: animate 2-

   .. code:: Ada

      procedure Swap (Values : in out Nat_Array; X, Y : Index)
        with
          Pre  => X /= Y,
          Post => Values = (Values'Old with delta
                              X => Values'Old (Y),
                              Y => Values'Old (X));

.. container:: animate 1-

   - Add a full functional contract to function :ada:`Index_Of_Minimum` and prove it

.. container:: animate 3-

   *Hint:* :ada:`Index_Of_Minimum` *contains a loop, so the prover is going to need help!*

.. container:: animate 4-

   .. code:: Ada

      function Index_Of_Minimum (Values : Nat_Array;
                                 From, To : Index)
                                 return Index
        with
          Pre  => To in From .. Values'Last,
          Post => Index_Of_Minimum'Result in From .. To and then
          (for all I in From .. To =>
             Values (Index_Of_Minimum'Result) <= Values (I));

   *This is not enough - you need to add a* :ada:`Loop_Invariant` *to the body*

.. container:: animate 5-

   .. code:: Ada

      for Index in From .. To loop
         if Values (Index) < Values (Min) then
            Min := Index;
         end if;
         pragma Loop_Invariant
           (Min in From .. To and then
              (for all I in From .. Index =>
                   Values (Min) <= Values (I)));
      end loop;

-----------------------------
Intermission - Permutations
-----------------------------

.. code:: Ada

   function Is_Sorted (Values : Nat_Array; From, To : Index) return Boolean is
     (for all I in From .. To - 1 => Values (I) <= Values (I + 1))
   with
     Ghost;

- This code is correct - an array is sorted if all elements are less than or
  equal to the next element

   - So the function will return True for all of these arrays:
     ``[1, 2, 3]``, ``[1, 1, 1]``, ``[1, 1, 3]``, ``[123, 231, 312]``

- For **proof**, when we sort an array, we need to know the contents of the array
  are the same but reordered

   - For input array ``[3, 2, 1]``, only ``[1, 2, 3]`` should be correct
   - So we need more than :ada:`Is_Sorted` - we need a way of making sure
     (prove) we have all the original elements and no new elements

- A **permutation** of a set is a rearrangement of the set where each element
  appears only once and no new elements are introduced

   - For this lab, there are two ways of implementing permutations

      - They can be found in sub-directories :filename:`answer1` and :filename:`answer2`
      - The following slides use :filename:`answer1`, but feel free to try
        :filename:`answer2` instead (or later)

   - Both methods can be considered "safe" for use in our proofs

----------------------
Selection Sort (1/3)
----------------------

.. container:: animate 1-

   - Add a functional contract to :ada:`Selection_Sort`

.. container:: animate 2-

   .. code:: Ada

      procedure Selection_Sort (Values : in out Nat_Array)
      with
        Post => Is_Sorted (Values)
          and then Is_Perm (Values'Old, Values);
      --  Upon completion, Values are a sorted version of input array

   *Again, this is not enough - we're dealing with loops*

.. container:: animate 3-

   + Add a loop invariant to procedure :ada:`Selection_Sort`

      - Actually two - one for the updated portion and one for the frame condition

.. container:: animate 4-

   .. code:: Ada

      pragma Loop_Invariant (Is_Sorted (Values, 1, Current));
      pragma Loop_Invariant
        (for all J in Current + 1 .. Values'Last =>
           Values (Current) <= Values (J));

   - And this isn't enough as well, because we're not taking care
     of our permutation ghost code

----------------------
Selection Sort (2/3)
----------------------

.. container:: animate 1-

   - Our permutation check inspects the ghost object :ada:`Permutation`

      - Whenever we swap values, we need to swap indexes in that object

   - Modify :ada:`Swap` to update :ada:`Permutation`

.. container:: animate 2-

   .. code:: Ada

      procedure Swap (Values : in out Nat_Array; X, Y : Index)
      is
         Temp        : Integer;
         Temp_Index  : Index with Ghost;
      begin
         Temp       := Values (X);
         Values (X) := Values (Y);
         Values (Y) := Temp;

         Temp_Index := Permutation (X);
         Permutation (X) := Permutation (Y);
         Permutation (Y) := Temp_Index;
      end Swap;

   *Also should update the postcondition to make sure we didn't*
   *break* :ada:`Permutation`

.. container:: animate 3-

   .. code:: Ada

      procedure Swap (Values : in out Nat_Array; X, Y : Index)
      with
        Pre  => X /= Y,
        Post => Values = (Values'Old with delta
                            X => Values'Old (Y),
                            Y => Values'Old (X))
          and then Permutation = (Permutation'Old with delta
                                    X => Permutation'Old (Y),
                                    Y => Permutation'Old (X));
   
----------------------
Selection Sort (3/3)
----------------------

.. container:: animate 1-

   * Now try to prove :ada:`Selection_Sort`

.. container:: animate 2-

   :color-red:`sort.ads:27:17: medium: postcondition might fail`

   :color-red:`sort.ads:27:17: cannot prove Is_Permutation_Array (Permutation)`

   :color-red:`sort.adb:71:1: possible fix: loop invariant at sort.adb:71 should mention Permutation`

   :color-red:`sort.ads:18:1: medium: in inlined expression function body at sort.ads:18`

   * Add a loop invariant to verify the permutation

      * Hint: It doesn't have to mention it directly - it can use :ada:`Is_Perm`
        which will be inlined

.. container:: animate 3-

   .. code:: Ada

      pragma Loop_Invariant (Is_Perm (Values'Loop_Entry, Values));

   * Running the proof again fails because we can't verify the first time through the loop

      :color-red:`sort.adb:75:33: medium: loop invariant might fail in first iteration`

   - We need to initialize :ada:`Permutation`

.. container:: animate 4-

   .. code:: Ada

      Permutation := (for J in Index => J);

   - Try proving it again

      - If it still doesn't prove, try increasing the :menu:`Proof level` in the dialog box
