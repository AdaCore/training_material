----------------------------
Advanced Flow Analysis Lab
----------------------------

- Find the :filename:`11_advanced_flow_analysis` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

-------------------
Flow Dependencies
-------------------

- Find and open the files :filename:`basics.ads` and :filename:`basics.adb` in :toolname:`GNAT Studio`

- Run :toolname:`GNATprove` in flow analysis mode

- Add flow dependency contracts to all subprograms except
  :ada:`Strange_Init_Rec` and :ada:`Strange_Init_Table`

  + Rerun :toolname:`GNATprove` in flow analysis mode
  + Discuss the correct flow dependencies of :ada:`Init_Table` with the instructor.

-----------------------------
Imprecise Flow Dependencies
-----------------------------

- Copy the flow dependencies of :ada:`Init_Rec` and :ada:`Init_Table` for
  respectively :ada:`Strange_Init_Rec` and :ada:`Strange_Init_Table`

- Run :toolname:`GNATprove` in flow analysis mode

  + Understand the error messages and add the suggested dependencies.

- Run :toolname:`GNATprove` in flow analysis mode

  + Do you understand the reason for the check messages?
  + Either adapt the flow dependencies or justify the messages with pragma :ada:`Annotate`
