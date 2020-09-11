----------------------------------------
Depends Contract and Flow Analysis Lab
----------------------------------------

- Find the :filename:`120_depends_contract_and_information_flow_analysis` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`default.gpr` in :toolname:`GNATstudio`

   + Or, on the command-line, do :command:`gnatstudio -Pdefault.gpr`

- Unfold the source code directory (.) in the project pane

.. container:: speakernote


   Lab should take about 30 minutes

----------------------------
Global / Depends Contracts
----------------------------

* Follow the instructions in the comments in :filename:`depends_exercise.ads`

   - First, add some `Global` contracts, and :menu:`Examine File`

      + **HINT:** If you start by setting `Input` and `Output` to `null` and then :menu:`Examine File`, :toolname:`GNATprove` will tell you where you are wrong!  (Of course, `Output` isn't legal for a SPARK function!)

   - Update the global contracts until there are no errors
   - Repeat the process for `Depends` contracts

.. container:: speakernote


   You'll get compile errors if the obvious contracts are missing
   You'll only get flow errors once you perform the analysis

---------------
Flow Analysis
---------------

* Try to flow-analyse (:menu:`Examine File`) the `Swap` and `Identity` procedures in :filename:`swapping.ads`

   - What is the problem?
   - What is the generated depends contract?

* Supply a suitable depends contract and check it using flow analysis.

---------------------------
Information Flow Analysis
---------------------------

* Package `Array_Swapping` contains annotated subprogram `Rotate3`

   - The function will swap around three elements within an array object
   - It uses a local subprogram `Swap`, which is not yet annotated for proof.

* Inspect the depends contracts and run the flow analysis

   - What is the result? Why?

* Add what you think is an appropriate contract to `Swap` - try to prove

   - Are the checks for `Swap` provable?

      + If not, consider why not, correct and repeat until a suitable, provable postcondition is arrived at.

* Are the checks for `Rotate3` provable?

   - If not, consider why not, go round the proof cycle until Rotate3 is provably correct.

      + **Hint**: Recall the sources of errors for a failed proof attempt (Like the code not doing what you expect it to!)
