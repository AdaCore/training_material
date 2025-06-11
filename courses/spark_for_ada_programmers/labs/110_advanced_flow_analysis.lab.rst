=====
Lab
=====

----------------------------
Advanced Flow Analysis Lab
----------------------------

- Find the :filename:`110_advanced_flow_analysis` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

------------------------
Flow Dependencies (1/2)
------------------------

.. container:: animate 1-

   - Find and open the files :filename:`basics.ads` and :filename:`basics.adb` in :toolname:`GNAT Studio`

   - Run :menu:`SPARK` |rightarrow| :menu:`Examine File`

      + Nothing exciting. No data dependencies have been specified.

   - Add flow dependency contracts to all subprograms except
     :ada:`Strange_Init_Rec` and :ada:`Strange_Init_Table`

.. container:: animate 2-

   *Example*

   .. code:: Ada

      procedure Swap (X, Y : in out Integer)
        with Global => null,
             Depends => (X => Y, Y => X);

------------------------
Flow Dependencies (2/2)
------------------------

.. container:: animate 1-

   - Rerun :menu:`SPARK` |rightarrow| :menu:`Examine File`

   - Fix any mistakes and repeat until analysis is successful

.. container:: animate 2-

   *Sample mistake*

   .. code:: Ada

      procedure Init_Table (T : out Table)
        with Global => null,
             Depends => (T => null);

   :color-red:`basics.ads:39:23: medium: missing self-dependency "T => T" (array bounds are preserved)`

   *Correct dependency*

   .. code:: Ada

      procedure Init_Table (T : out Table)
        with Global => null,
             Depends => (T => +null);

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
