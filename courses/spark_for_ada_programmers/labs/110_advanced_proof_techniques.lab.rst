-------------------------------
Advanced Proof Techniques Lab
-------------------------------

- Find the :filename:`110_advanced_proof_techniques` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`default.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -Pdefault.gpr`

- Unfold the source code directory (.) in the project pane
- This exercise was taken directly out of a FoVeOOS competition (Formal Verification of Object-Oriented Software)

   + Participants were given an algorithm that finds the maximum element of an array, in a rather unconventional way, and they were asked to prove the algorithm correct.

.. container:: speakernote


   Lab should take about 30 minutes

---------------
Proving Loops
---------------

* Loop Invariants and Correctness

  - Find and open the file :filename:`max.adb`
  - Add a suitable loop invariant and prove the correctness of the loop.

* Loop Variants and Termination

  - Keep open the file :filename:`max.adb`
  - Add a suitable loop variant and prove that the loop terminates.

* **HINT:** Ghost code, will make the loop invariant easier to read

   - But it's not required!

.. container:: speakernote


   The loop invariant needs to prove that there is no value outside of x..y that is larger than either of the values at x and y
