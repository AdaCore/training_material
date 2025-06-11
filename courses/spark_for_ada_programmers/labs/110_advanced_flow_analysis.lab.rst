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

-----------------------------------
Imprecise Flow Dependencies (1/2)
-----------------------------------

.. container:: animate 1-

   - Copy the flow dependencies of :ada:`Init_Rec` to :ada:`Strange_Init_Rec`
   - Perform flow analysis and examine the result

.. container:: animate 2-

   :color-red:`basics.ads:51:11: error: parameter "Cond" is missing from input dependence list`

   :color-red:`basics.ads:51:11: error: add "null => Cond" dependency to ignore this input`

   **Cond** *is a parameter, so it must be added to the dependency contract*

   - Fix the dependency contract and rerun flow analysis

.. container:: animate 3-

   :color-red:`basics.ads:51:18: medium: missing dependency "R => Cond"`

   :color-red:`basics.ads:52:26: medium: incorrect dependency "null => Cond"`

   *Initialization of parameter* **R** *is path-dependent, and that path is*
   *controlled by* **Cond** *- so it must be listed as a dependency of* **R**

   - Fix the dependency contract and rerun flow analysis

.. container:: animate 4-

   *Note that by adding* **Cond** *as a dependency of* **R**, *we no longer*
   *need an entry specifically for* **Cond**

   .. code:: Ada

      procedure Strange_Init_Rec (R : out Rec; Cond : Boolean)
        with Global => null,
             Depends => (R => Cond);

-----------------------------------
Imprecise Flow Dependencies (2/2)
-----------------------------------

.. container:: animate 1-

   - Copy the flow dependencies of :ada:`Init_Table` to :ada:`Strange_Init_Table`
   - Perform flow analysis and examine the result

.. container:: animate 2-

   *Same problem as before - missing a dependency contract for* **Val**.

   - Fix the dependency contract and rerun flow analysis

.. container:: animate 3-

   :color-red:`basics.ads:55:18: medium: missing dependency "T => Val"`

   :color-red:`basics.ads:56:25: medium: incorrect dependency "null => Val"`

   *Remember, even though we can see that* **T (T'First)** *doesn't actually depend on*
   **Val,** *flow analysis does not look at array index values - so it assumes*
   *there is a dependency*

.. container:: animate 4-

   .. code:: Ada

      procedure Strange_Init_Table (T : out Table; Val : Integer)
      with Global => null,
        Depends => (T => +Val);
