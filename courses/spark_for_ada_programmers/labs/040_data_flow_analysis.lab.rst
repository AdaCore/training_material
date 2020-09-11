-------------------------
Data Flow Analysis Lab
-------------------------

- Find the :filename:`040_data_flow_analysis` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`default.gpr` in :toolname:`GNATstudio`

   + Or, on the command-line, do :command:`gnatstudio -Pdefault.gpr`

- Unfold the source code directory (.) in the project pane 

.. container:: speakernote


   Total lab time should be 5 minutes

----------------------
Flow Errors Exercise
----------------------

- Find and open the files :filename:`dfa.ads` and :filename:`dfa.adb` in :toolname:`GNATstudio`
- Study the code and see if you can predict what's wrong.

   + These examples illustrate the basic forms of data-flow analysis in SPARK.

- Use :menu:`SPARK` |rightarrow| :menu:`Examine File...` to analyse the body of package `DFA`.
- Click on the "Locations" tab to see the messages organised by unit.
- Make sure you understand the errors and warnings that :toolname:`GNATprove` produces.

   + Discuss these with the course instructor.
