
***************************
Advanced Proof Techniques
***************************
.. |rightarrow| replace:: :math:`\rightarrow`

==============
Introduction
==============

--------------
Introduction
--------------

* Language contains features to support debug / proof
* Ghost Code

   - Ability to add code that can be disabled at delivery

* Loop pragmas

   - Specialized pragmas to deal with provers inability to track loop iterations

============
Ghost Code
============

------------
Ghost Code
------------

* Ghost code is normal Ada code that is only used for specification

   - Ghost code should have no effect on the behavior of the program

   - When the program is compiled with assertions, ghost code is executed like normal code
   - It can also be ignored by the compiler

* Ghost entities should be marked with the Ghost aspect

.. code:: Ada

   procedure Do_Something (X : in out T) is
     X_Init : constant T := X with Ghost;
   begin
     Do_Some_Complex_Stuff (X);
     pragma Assert (Is_Correct (X_Init, X));
     -- It is OK to use X_Init inside an assertion.
     X := X_Init; --  Compilation error:
     -- Ghost entity cannot appear in this context

.. container:: speakernote

   As the properties we want to specify grow more complex, the need can arise for entities that are only used for specification purposes
   In a qualification process, it may be important to make sure that these new entities cannot affect the behavior of the program, or even to ensure they are removed from production code
   This concept, usually called Ghost code, is supported in SPARK by the mean of a new Ghost aspect
   It can be used to annotate any normal entity, such as variables, types, subprograms, packages... If an entity is marked as Ghost, GNATprove will make sure that it cannot affect the program's behavior
   To be able to dynamically test the contracts using it, ghost code will be executed like normal code when the program is compiled with assertions enabled
   The compiler can also be instructed not to generate code for ghost entities.
   As an example, to allow specifying the DoSomething procedure, we store the initial value of X in a ghost constant XInit.
   We can then reference this variable from assertions to check that the computation performed by the call to DoSomeComplexStuff modified the value of X in the expected manner.
   However XInit should not be used in normal code, for example to restore the initial value of X.
   In our example, the use of XInit will be flagged as illegal by the compiler.
   Note that more complex cases of interference between ghost and normal code may only be detected by running GNATprove.

-----------------
Ghost Functions
-----------------

* Ghost functions express properties only used in contracts

   - Expression functions used to factor out common expression in contracts can be marked as ghost
   - Ghost functions are OK to be inefficient as long as assertions are disabled in the final executable

.. code:: Ada

   type Stack is private;
   function Get_Model (S : Stack) return Nat_Array
      with Ghost;
   --  Returns an array as a model of a stack.
   procedure Push (S : in out Stack; E : Natural) with
     Pre  => Get_Model (S)'Length < Max,
     Post => Get_Model (S) = Get_Model (S)'Old & E;
   function Peek (S : Stack; I : Positive)
      return Natural
   is
     -- Get_Model cannot be used in this context.
     (Get_Model (S) (I));

------------------------
Global Ghost Variables
------------------------

* Global ghost variables store information that is only useful for specification. They can be used to

   - Maintain a model of a complex or private data-structure

   - Specify properties over several runs of subprograms
   - Act as placeholders for intermediate values of variables

.. code:: Ada

   Last_Accessed_Is_A : Boolean := False with Ghost;
   procedure Access_A with
     Post => Last_Accessed_Is_A;
   procedure Access_B with
     Pre  => Last_Accessed_Is_A,
     Post => not Last_Accessed_Is_A;
     --  B can only be accessed after A
   V_Interm : T with Ghost;
   procedure Do_Two_Things (V : in out T) with
     Post => (First_Thing_Done (V'Old, V_Interm)
          and Second_Thing_Done (V_Interm, V));

.. container:: speakernote

   Though it happens less often, specification may require storing additional information into global variables.
   As this information is not needed in normal code, these global variables should be marked as ghost so that they can be erased by the compiler.
   These variables can be used for various reasons.
   A rather common case is to specify programs modifying a complex or private global data-structure by providing a model for it, that is updated by the program along with the data-structure.
   Global variables can also store information about previous runs of subprograms in order to specify simple temporal properties.
   In our example, we have two procedures, one to access a state A and the other to access a state B.
   The global variable LastAccessedIsA is used to specify that B cannot be accessed twice without accessing A in between.
   Finally, it can be the case that the requirements of a subprogram expresses its expected behavior as a sequence of actions to be performed.
   To write this kind of specification more easily, global ghost variables may be used to store intermediate values of variables in the program.
   For example, we specify DoTwoThings in two steps using the global variable VInterm to store the intermediate value of V between the two things to be done.
   Note that, conceptually, this usage could be expressed using an existential quantification on the variable VInterm.
   This cannot always be done in SPARK as quantification in Ada is restricted to for loop patterns.
   What is more, supplying the value of the variable may help the prover to effectively verify the contracts.

-----------------------
Local Ghost Variables
-----------------------

* Local ghost variables or constants store information that is only useful in intermediate assertions

   - Store the intermediate values of variables or expressions
   - Construct iteratively a data structure that reflects a property of the subprogram

.. code:: Ada

   procedure P (X : in out T) with Post => F (X, X'Old)
   is
     X_Init : constant T := X with Ghost;
   begin
     if Condition (X) then
       ...
       pragma Assert (F (X, X_Init));
   ...
   procedure Sort (A : in out Nat_Array) with
     Post => (for all I in A'Range =>
                (for some J in A'Range => A (I) = A'Old (J)))
   is
     Permutation : Index_Array := (1 => 1, 2 => 2, ...) with
       Ghost;

.. container:: speakernote

   Just like for specifications, ghost code can be used to enhance what can be expressed inside intermediate assertions.
   In particular, local variables or constants whose only purpose is to serve in assertions are a common occurrence.
   Most of the time, these variables are used to store previous values of variables or expressions to which we want to refer in our assumptions.
   They are especially useful to refer to initial values of parameters and expressions which cannot be accessed using the 'Old attribute outside of the subprogram's postcondition.
   In our example, to help GNATprove discharge the postcondition of P, we want to assert that it holds separately in every branch of an if statement.
   Since in these assertions, unlike in P's postconditions, we cannot use the 'Old attribute to access the initial value of the parameter X, we have resorted to introducing a local ghost constant XInit for this value.
   Local ghost variables can also be used for more complex things such as building a data-structure that serves as witness of a complex property of the subprogram.
   In our example, we want to prove that the Sort procedure do not create new elements, that is, all the elements that are in A after the sort were already in A before the sort.
   The property is rather complex for a prover to verify as it involves an alternation of quantifiers.
   To help the prover, it may be interesting to store, for each index I, an index J that has the expected property.

------------------
Ghost Procedures
------------------

* Ghost procedures can only affect the values of Ghost variables

   - They can be used to abstract away complex treatment on ghost variables

   - Or to group together intermediate assertions

   - In normal code, the only statements that can refer to ghost entities are assignments to ghost variables and ghost procedure calls

.. code:: Ada

   A : Nat_Array := ... with Ghost;
   procedure Increase_A with Ghost is
   begin
     for I in A'Range loop
       A (I) := A (I) + 1;
     end loop;
   end Increase_A;
   procedure Prove_P (X : T) with Ghost,
     Global => null,
     Post   => P (X);

.. container:: speakernote

   Ghost procedures cannot affect the value of normal variables.
   Therefore, they are mostly used to perform treatments on ghost variables or to group together a set of intermediate assertions.
   Abstracting away treatment of ghost variables or assertions inside a ghost procedure has several advantages.
   First, it enhances expressivity as, to simplify the removal of ghost code by the compiler, the only ghost statements that are allowed to appear in normal code are assignments to ghost variables and ghost procedure calls.
   As an example, the for loop contained in IncreaseA could not appear by itself in normal code.
   Then, it improves readability by hiding away complex code that is of no use for the functional behavior of the subprogram.
   Finally, it can help GNATprove by abstracting away assertions that would otherwise pollute its context.
   For example, calling ProveP on X will only add P (X) to the proof context instead of the possible important set of assertions that are required to verify it.
   What is more, the proof of P will only be done once and may be made easier by the fact that no unnecessary information is present in the context while verifying it.
   Also, if ProveP happens to not be fully verified, the remaining assumptions will be reviewed more easily if they are in a small context.

=======
Loops
=======

---------------
Proving Loops
---------------

* Correctness

   - Loop Invariant

* Termination

   - Loop Variant

* Loop entry values

.. container:: speakernote

   Formal verification tools like GNATprove rely on two main inputs from programmers:
   subprogram contracts (preconditions and postconditions)
   loop invariants

----------------
Loop Invariant
----------------

* A special assertion
* Expressed as a `pragma`

.. code:: Ada

   function Get_Prime (Low, High : Positive) return Natural is
     J : Positive := Low;
   begin
     while J <= High loop
       if Is_Prime (J) then
         return J;
       end if;
       pragma Loop_Invariant
         (J in Low .. High
           and
         (for all K in Low .. J => not Is_Prime (K)));
       J := J + 1;
     end loop;
     return 0;
   end Get_Prime;

----------------
Loop Invariant
----------------

* Dynamic Semantics

   - Executed just like a regular assertion

* Static Semantics

   - :toolname:`GNATprove` proves the loop invariant in two stages:

   - It proves first that the loop invariant is true at the first iteration.

      .. code:: console

         loopinv.adb:33:7: info:
            loop invariant initialization proved

   - It then proves that, assuming the loop invariant held at the previous iteration, it still holds at the next iteration.

      .. code:: console

         loopinv.adb:33:7: info:
            loop invariant preservation proved

----------------
Loop Invariant
----------------

* Similar to (mathematical) induction

   - Prove that a property holds for the base case

      + for example:  Prop (0)

   - Prove that the property holds for an arbitrary step

   -  for example:

      .. code:: Ada

         (for all N in Integer => Prop(N) => Prop(N + 1))

* Important: to be useful, the loop invariant needs to be strong enough to prove the post condition *of the loop*

* Loop invariant can be placed anywhere

   - Except in branches
   - Slightly different than classical Hoare loop invariant

--------------
`Loop_Entry`
--------------

* Attribute `'Loop_Entry` is useful for referring to the value of a variable on entry to the loop

      .. code:: Ada

         type Array_T is array (1 .. 10) of Integer range 0 .. 7;
         ...
         for I in A'Range loop
            Result := Result + A(I);
            pragma Loop_Invariant (
               Result <= Result'Loop_Entry + 7 * I);
         end loop;

* `'Loop_Entry(Loop_Name)` specifies the name of the loop being referred to, when in nested loops

-------------------------------------
Loop Proof in :toolname:`GNATprove`
-------------------------------------

* :toolname:`GNATprove` to prove iterations around the (virtual) loop formed by the following steps:

   - Take any context satisfying the loop invariant, which summarizes all previous iterations of the loop.
   - Execute the end of a source loop iteration (just the increment here).
   - Test whether the loop exits, and continue with values which do not exit.
   - Execute the start of a source loop iteration (just the if-statement here).
   - Check that the loop invariant still holds.

.. container:: speakernote

   Let us have a closer look at what is going on inside GNATprove during loop proof.

----------------------------------------------
Steps of Loop Proof in :toolname:`GNATprove`
----------------------------------------------

.. code:: Ada

  function Get_Prime (Low, High : Positive) return Natural is
    J : Positive := Low;
  begin
    while J <= High loop -- (3)
      if Is_Prime (J) then -- (4)
        return J;
      end if;
      pragma Loop_Invariant -- (1)
        (J in Low .. High and (for all K in Low .. J =>
           not Is_Prime (K))); --(5)
      J := J + 1; --(2)
    end loop;
    return 0;
  end Get_Prime;

1. Take any context satisfying the loop invariant, which summarizes all previous iterations of the loop.
2. Execute the end of a source loop iteration (just the increment here).
3. Test whether the loop exits, and continue with values which do not exit.
4. Execute the start of a source loop iteration (just the if-statement here).
5. Check that the loop invariant still holds.

-------------------------
Writing Loop Invariants
-------------------------

* Understand

   - General theory behind loop invariants
   - Semantics of the programming language used

   - Specific application of the general theory to the programming language used in a given tool, and

   - Detailed knowledge of what the loop performs

-------------------------
Writing Loop Invariants
-------------------------

* General Theory

   - A loop invariant should provide all the necessary information about variables modified in the loop, which is otherwise lost for proving properties inside the loop (including the loop invariant itself) and after the loop.

* Language Semantics

   - Between two iterations of the loop, the only information available for variables written to directly in the loop, or through calls inside the loop (including global output variables, and output parameters), and only those, is what the loop invariant mentions.

-------------------------
Writing Loop Invariants
-------------------------

* :toolname:`GNATprove`

   - The logical encoding of SPARK additionally preserves some information about modified variables across loop iterations: the range of a for-loop, the condition of a while-loop, the bounds of an array variable, the discriminants of a record variable.

* Loop Behavior

   - The programmer should mention in the loop invariant not only what properties the loop modifies, but also what properties the loop maintains up to the nth iteration, when these properties involved variables modified in the loop.

---------------------------------
Loop Invariants - Ingredients
---------------------------------

* Four properties of a good loop invariant, in increasing order of difficulty:

   - **INIT**

      + It should be provable in the first iteration of the loop.

   - **INSIDE**

      + It should allow proving absence of run-time errors and local assertions inside the loop.

   - **AFTER**

      + It should allow proving absence of run-time errors, local assertions and the subprogram postcondition after the loop.

   - **PRESERVE**

      + It should be provable after the first iteration of the loop.

--------------------------
Loop Invariants - Recipe
--------------------------

* Always start with the hardest constraint...
* ... and the easiest check!

   1. Start by inspecting the post condition of the loop
   2. Study the terminating condition of the loop

   3. What are the loop variables?

      + Their value on termination?

   4. Generalize postcondition for loop invariant
   5. Check the loop initialization
   6. Check the preservation step
   7. Iterate steps 4-6 if necessary

.. container:: speakernote

   Framing problem! Is enough known about the loop variables?

---------------------------
Does your loop terminate?
---------------------------

* `Loop_Variant`

   - Allows proof of loop termination by specifying expressions that will increase or decrease on each iteration

.. code:: Ada

   procedure P is
      type Total is range 1 .. 100;
      subtype T is Total range 1 .. 10;
      I : T := 1;
      R : Total := 100;
   begin
      while I < 10 and R > 10 loop
          pragma Loop_Invariant (I <= 5);
          pragma Loop_Variant (Increases => I,
                               Decreases => R);
          R := R - I;     -- 100, 99, 97, 94, 90, 85, 80, ...
          if I < 5 then
             I := I + 1;  -- 1, 2, 3, 4, 5, 5, 5, 5, 5...
          end if;
      end loop;
   end P;

.. container:: speakernote

   There is an example of this in :toolname:`GNAT Studio` we can look at if we want to.
   The sharper students might point out that the 'Increases I' isn't actually needed and you can still prove termination with just 'Decreases R'.
   This is true, and it would be nice to come up with a better example!
   You could do something such as changing the first assignment to:
   if I `>` 3 then
   R := R - I;     -- 100, 99, 97, 94, 90, 85, 80, 75, 70...
   end if;
   In order to make both parts of the loop variant necessary.

----------------
`Loop_Variant`
----------------

* Rules for where to place: same as for `Loop_Invariant`
* Must be at least one instance of Increases (or Decreases) followed by an expression
* Additional instances of Increases/Decreases expressions may appear, processed in textual order:

   - Check is made that the first expression increases (or decreases) for this iteration, or stays the same
   - If it stays the same for this iteration then the next expression is checked, and so on
   - As long as one of the expressions can be shown to be changing (progressing) in the specified direction for each iteration then termination can be proved

========
Lab
========

.. include:: labs/110_advanced_proof_techniques.lab.rst

=========
Summary
=========

----------
Summary
----------

* Ghost Code

   - Objects to maintain state information
   - Subprogram to modularize queries and debugging steps

* Loop pragmas

   - Prove loop is valid for all iterations of loop
   - Ensure loop terminates
