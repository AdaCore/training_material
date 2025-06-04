=====
Lab
=====

-------------------
Flow Analysis Lab
-------------------

- Find the :filename:`050_flow_analysis` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

----------------------------------------
Aliasing and Initialization - Messages
----------------------------------------

.. container:: animate 1-

   - Find and open the files :filename:`basics.ads` and :filename:`basics.adb` in :toolname:`GNAT Studio`
   - Study the code and see if you can predict what's wrong.

      + These examples illustrate the basic forms of flow analysis in SPARK.

   - Use :menu:`SPARK` |rightarrow| :menu:`Examine File` to analyze the body of package `Basics`.
   - Click on the :menu:`Locations` tab to see the messages (organized by unit).
   - Make sure you understand the check messages that :toolname:`GNATprove` produces.

.. container:: animate 2-

   ::

      basics.adb:17:13: medium: formal parameters "X" and "Y" might be aliased
      basics.ads:25:26: medium: "T" might not be initialized in "Init_Table"

   * We want to fix the code, or add an annotation to prevent the messages

      * We do not want any messages from our analysis.

-------------------------------------
Aliasing and Initialization - Fixes
-------------------------------------

.. container:: animate 1-

   * Problem 1 - ``formal parameters "X" and "Y" might be aliased``

      * Hint: if we prevent **Swap** from being called when **I** and **J** are equal,
        we can safely add an anotation indicating this is a false positive

.. container:: animate 2-

   .. code:: Ada

      if I /= J then
         Swap (T (I), T (J));
         pragma Annotate (GNATprove, False_Positive,
                          "formal parameters * might be aliased",
                          "I /= J so T(I) and T(J) cannot alias");
      end if;

.. container:: animate 1-

   * Problem 2 - ``"T" might not be initialized in "Init_Table"``

      * Hint: We need to initialize the array in a way that the analysis
        knows the entire array was initialized

.. container:: animate 3-

   .. code:: Ada

      T := (others => 0);
      T (T'First) := 1;
      T (T'Last) := 2;

----------------------------
Generated Global Contracts
----------------------------

.. container:: animate 1-

   - Now that you've performed flow analysis, you can examine the generated global contracts

      * Right-click in the package spec and select :menu:`SPARK` |rightarrow| :menu:`Globals`
        |rightarrow| :menu:`Show generated Global contracts`.

   - Study the generated contracts and make sure you understand them.

.. container:: animate 2-

   - Output

      - Subprograms with *The* in the name are modifying global data (e.g. **Init_The_Table**)

         - So the global contract uses the fully qualified name of the object being modified

      - Remaining subprograms have no interaction with global data

         - So the global contract indicates *null*

---------------------------------
Adding Our Own Global Contracts
---------------------------------

.. container:: animate 1-

   - Add a null data dependency contract to all subprograms

      - This isn't correct, but we're proving a point.

.. container:: animate 2-

   - Example

      .. code:: Ada

         procedure Swap_Rec (R : in out Rec)
           with Global => null;

   - Now run :menu:`Examine File` again

.. container:: animate 3-

   ::

      high: "The_Rec" must be listed in the Global aspect of "Swap_The_Rec"
      high: "The_Table" must be listed in the Global aspect of "Swap_The_Table"
      high: "The_Rec" must be listed in the Global aspect of "Init_The_Rec"
      high: "The_Table" must be listed in the Global aspect of "Init_The_Table"

   * Analysis shows global data has been modified in these subprograms.

      * Add the appropriate contracts

.. container:: animate 4-

   .. code:: Ada

      procedure Swap_The_Rec
        with Global => (In_Out => Basics.The_Rec);
      procedure Swap_The_Table (I, J : Index)
        with Global => (In_Out => Basics.The_Table);
      procedure Init_The_Rec
        with Global => (Output => Basics.The_Rec);
      procedure Init_The_Table
        with Global => (Output => Basics.The_Table);

-------------------
Verifying Results
-------------------

* Sometimes we want acknowledgement of our work

   * A "verbose" indication that our contracts and annotations are correct

* Rerun :menu:`Examine File` but now click the :menu:`Report checks proved` button

.. container:: animate 2-

   ::

      basics.adb:12:14: info: non-aliasing of formal parameters "X" and "Y" proved
      basics.adb:18:16: info: justified that formal parameters "X" and "Y" might be aliased
      basics.ads:11:11: info: data dependencies proved
      basics.ads:17:11: info: data dependencies proved
      basics.ads:20:11: info: data dependencies proved
      basics.ads:23:11: info: data dependencies proved
      basics.ads:26:11: info: data dependencies proved
      basics.ads:28:24: info: initialization of "R" proved
      basics.ads:29:11: info: data dependencies proved
      basics.ads:31:26: info: initialization of "T" proved
      basics.ads:32:11: info: data dependencies proved
      basics.ads:35:11: info: data dependencies proved
      basics.ads:35:38: info: initialization of "The_Rec" proved
      basics.ads:38:11: info: data dependencies proved
      basics.ads:38:38: info: initialization of "The_Table" proved

