-----------------
Interfacing Lab
-----------------

- Find the :filename:`160_interfacing` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`default.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -Pdefault.gpr`

- Unfold the source code directory (.) in the project pane
- This is a set of exercises demonstrating how to interface to external, volatile state.

.. container:: speakernote


   Lab should take about 15 minutes

-----------------------------------
External Variables and Properties
-----------------------------------

* Add aspects to the declaration of the external state variable Port to indicate:

   - That it is volatile
   - What sort of inputs/outputs it has.
   - Which address to be used for the port

* Add `Global` and `Depends` contracts to the subprogram  declarations in the package specification.

   - Use the SPARK tools to check your contracts.
   - **HINT:** If you read `Port`, it changes (is modified)
