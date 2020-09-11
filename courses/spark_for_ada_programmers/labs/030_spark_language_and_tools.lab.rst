------------------------------
SPARK Language and Tools Lab
------------------------------

- Find the :filename:`030_spark_language_and_tools` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`default.gpr` in :toolname:`GNATstudio`

   + Or, on the command-line, do :command:`gnatstudio -Pdefault.gpr`

- Unfold the source code directory (.) in the project pane

.. container:: speakernote


   These exercises should take < 5 minutes each. Total lab time should be 15 minutes

--------------------------
Language Subset Exercise
--------------------------

- Double-click on :filename:`subset.ads`
- Use :menu:`SPARK` |rightarrow| :menu:`Examine File...` to analyse the specification of package `Subset`.
- Click on the :menu:`Locations` tab to see the messages organised by unit.

-----------------------
Side Effects Exercise
-----------------------

- Find and open the file :filename:`side_effects.adb`
- Study the code and see if you can understand what's wrong before running the SPARK Tools.
- Use :menu:`SPARK` |rightarrow| :menu:`Examine File...` to analyse the body of package `Side_Effects`.

   - Make sure you understand the errors and warnings that :toolname:`GNATprove` produces.
   - Discuss these with the course instructor.

- Use the menu item :menu:`Build` |rightarrow| :menu:`Project` |rightarrow| :menu:`Build & Run` |rightarrow| :menu:`stest.adb` to build and run this lab's test program.
- Would you expect the results to be the same on all machines?

.. container:: speakernote


   If you don't get a constraint error on execution, try reversing the order of the operations

-------------------
Aliasing Exercise
-------------------

- Find and open the files :filename:`aliasing.ads` and :filename:`aliasing.adb`. Study the code - see if you can predict what's wrong.
- Use :menu:`SPARK` |rightarrow| :menu:`Examine File...` to apply :toolname:`GNATprove` to the body of package `Aliasing`.

   * Make sure you understand the errors and warnings that :toolname:`GNATprove` produces.
   * Discuss these with the course instructor.

- Use the menu item :menu:`Build` |rightarrow| :menu:`Project` |rightarrow| :menu:`Build & Run` |rightarrow| :menu:`atest.adb` to build and run this lab's test program.
- Does it behave as expected?
- *Extra Credit*: What might happen if you added an extra field to the record type `Aliasing.Rec`? Why?

.. container:: speakernote


   If you add an extra field, you need to update the aggregatate assignments
   Pro: Now the initial assignment has an effect (initializing new parameter)
   Con: There's nothing forcing Multiply to deal with new parameter
   So doing the assignment as an aggregate is the real solution
