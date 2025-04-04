=======
Loops
=======

-----------------
Unrolling Loops
-----------------

* :toolname:`GNATprove` can unroll loops when:

  - Loop is of the form :ada:`for J in A .. B loop`
  - Number of iterations is less than 20
  - The only local variables declared in the loop are scalars

* Confirming message issued when using switch :command:`--info`

  .. code:: console

     info: unrolling loop

* Strongest Postcondition calculus can deal with unrolled loop

  - But size of code might become large
  - Especially on nested loops

* Loop unrolling can be prevented

  - Globally with switch :command:`--no-loop-unrolling`
  - On a specific loop with a loop invariant

-----------------
Loop Invariants
-----------------

* A :dfn:`loop invariant` is a special assertion

  - Placed inside loops
  - Executed like an assertion at runtime
  - Interpreted specially in proof
  - Slightly different from classical Hoare loop invariant

* Dynamic checks inserted by GNAT

  - When using switch :command:`-gnata`
  - Or pragma :ada:`Assertion_Policy (Loop_Invariant => Check)`

* Multiple loop invariants are allowed

  - Must be grouped
  - Same as conjunction of conditions using :ada:`and`

* Placement anywhere in the top-level sequence of statements

  - Typically at the beginning or end of the loop
  - Can be inside the statements of a *declare block*
  - Default loop invariant of :ada:`True` at beginning of the loop

--------------------------
Loop Invariants in Proof
--------------------------

* The loop invariant acts as a cut point for the SP calculus

  - Establish it at the beginning of the loop
  - Check that it is preserved by one iteration
  - Assume it to check the remaining of the program

.. image:: loop_invariants.png

------------------------------
Placement of Loop Invariants
------------------------------

.. container:: columns

 .. container:: column

    * Proof reasons around the *virtual loop*

      - Starting from the loop invariant
      - Ending at the loop invariant

 .. container:: column

    .. image:: loop_invariants_placement.png

------------------------------------------
Four Properties of a Good Loop Invariant
------------------------------------------

* These four properties should be established in this order

* [INIT] - It should hold in the first iteration of the loop

   - :toolname:`GNATprove` generates a loop invariant initialization check

* [INSIDE] - It should allow proving absence of run-time errors and local
  assertions inside the loop

* [AFTER] - It should allow proving absence of run-time errors, local
  assertions and the subprogram postcondition after the loop

* [PRESERVE] - It should be preserved by the loop

   - :toolname:`GNATprove` generates a loop invariant preservation check

-----------------------
Summarizing Mutations
-----------------------

* Analysis of arbitrary loop iteration in coarse context

  - All information on modified variables is lost
  - Except information preserved in the loop invariant

* Example: initialization loop

  .. code:: ada

     procedure Init_Table (T : out Table)
     with
       Post => (for all J in T'Range => T(J) = 0);

     procedure Init_Table (T : out Table) is
     begin
        for J in T'Range loop
           T(J) := 0;
           pragma Loop_Invariant
             (for all K in T'First .. J => T(K) = 0);
        end loop;
     end Init_Table;

--------------------------
Accumulating Information
--------------------------

* Analysis of arbitrary loop iteration in coarse context

  - All information accumulated on variables is lost
  - Except information preserved in the loop invariant

* Example: search loop

  .. code:: ada

     procedure Search_Table (T : Table; Found : out Boolean)
     with
       Post => Found = (for some J in T'Range => T(J) = 0);

     procedure Search_Table (T : Table; Found : out Boolean) is
     begin
        for J in T'Range loop
           if T(J) = 0 then
              return True;
           end if;
           pragma Loop_Invariant
             (for all K in T'First .. J => T(K) /= 0);
        end loop;
        return False;
     end Search_Table;

------------------------------
Attribute :ada:`Loop_Entry`
------------------------------

* Attribute :ada:`Loop_Entry` used to refer to the value of a variable on
  entry to the loop

  .. code:: ada

     procedure Bump_Table (T : in out Table) is
     begin
        for J in T'Range loop
           T(J) := T(J) + 1;
           pragma Loop_Invariant
             (for all K in T'First .. J => T(K) = T'Loop_Entry(K) + 1);
        end loop;
     end Bump_Table;

* Similar to attribute :ada:`Old` which is usable only inside postconditions

  - In many cases, :ada:`X'Loop_Entry` is also value on subprogram entry
  - Same limitations as for attribute :ada:`Old`

    + Use :ada:`pragma Unevaluated_Use_Of_Old (Allow)` if needed

* Use :ada:`X'Loop_Entry(Loop_Name)` for value of :ada:`X` on entry to loop
  not directly enclosing

----------------------------
Loop Frame Condition (1/2)
----------------------------

* Reminder: analysis of arbitrary loop iteration in coarse context

  - All information on modified variables is lost
  - Except information preserved in the loop invariant

|

* This is true for the :dfn:`loop frame condition`

  - Variables that are not modified
  - Parts of modified variables that are preserved
  - Similar to frame condition on subprogram calls

|

* :toolname:`GNATprove` generates part of the frame condition

  - Variables that are not modified, or only on paths that exit the loop
  - Components of records that are not modified
  - Components of arrays that are not modified

    + When the array is only assigned at the current loop index

----------------------------
Loop Frame Condition (2/2)
----------------------------

* In other cases, explicit frame condition might be needed

* Typically use attribute :ada:`Loop_Entry`

  .. code:: ada

     procedure Bump_Table (T : in out Table) is
     begin
        for J in T'Range loop
           T(J) := T(J) + 1;
           pragma Loop_Invariant
             (for all K in J .. T'Last =>
                (if K > J then T(K) = T'Loop_Entry(K)));
        end loop;
     end Bump_Table;

.. container:: speakernote

   We don't use "(for all K in J+1 .. T'Last =>" here, as that could
   lead to an index overflow.
   Hence the use of an if-expression.

---------------------------
Classical Loop Invariants
---------------------------

* Known best loop invariants for some loops

  - Initialization loops - initialize the collection
  - Mapping loops - map each component of the collection
  - Validation loops - check each component of the collection
  - Counting loops - count components with a property
  - Search loops - search component with a property
  - Maximize loops - search component that maximizes a property
  - Update loops - update each component of the collection

|

* SPARK User's Guide gives detailed loop invariants

  - See section *7.9.2 Loop Examples*
  - Loops on arrays or formal containers

-----------------------------
Quiz: Non-terminating Loops
-----------------------------

What's wrong with the following code?

.. code:: ada

   loop
      null;
   end loop;
   pragma Assert (False);

.. container:: animate

   * Loop does not terminate

   * :toolname:`GNATprove` proves the assertion of :ada:`False`!

     - Because that program point is unreachable (dead code)

   * :toolname:`GNATprove` implements defense in depth

     - Non-terminating loop causes enclosing subprogram to also not terminate
     - Switch :command:`--proof-warnings=on` can detect dead code
     - Proof of loop termination based on loop variants

---------------------
Loop Variants (1/2)
---------------------

* A :dfn:`loop variant` is a special assertion

  - Placed inside loops
  - Executed specially at runtime
  - Interpreted specially in proof

|

* Dynamic checks inserted by GNAT

  - When using switch :command:`-gnata`
  - Or pragma :ada:`Assertion_Policy (Loop_Variant => Check)`
  - Check that expression varies as indicated at each iteration

|

* Only one loop variant is needed to prove loop termination

  - And only on *while loop* or *plain loop*, not on *for loop*

|

* Same placement as for loop invariants

  - Must be grouped if both presents

---------------------
Loop Variants (2/2)
---------------------

* Same syntax as subprogram variants

  .. code:: ada

     procedure Bump_Table (T : in out Table) is
        J : Index'Base := T'First;
     begin
        while J <= T'Last loop
           T(J) := T(J) + 1;
           J := J + 1;
           pragma Loop_Variant (Increases => J);
        end loop;
     end Bump_Table;

* Could also use :ada:`(Decreases => -J)`

* Same loop variant could be placed anywhere in the loop here

  - Because check between two successive evaluations of the variant
  - The loop invariant must be modified to reflect current values

