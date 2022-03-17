------------------------
State Abstraction Lab
------------------------

- Find the :filename:`130_state_abstraction` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory are three folders, one for each exercise.

   - For a particular exercise, navigate to the appropriate folder and then open the :filename:`default.gpr` using :toolname:`GNAT Studio`

      * Or, on the command-line in the appropriate folder:

        | :command:`gnatstudio -Pdefault.gpr`

- Unfold the source code directory (.) in the project pane
- This set of exercises will familiarize you with using state abstractions

.. container:: speakernote


   Lab should take about 20 minutes

----------
Overview
----------

- In these folders there are a number of different declarations of package `A_Stack`
- Each of these folders will show how to deal with the *state* of data

   + Using just **Global** contracts
   + Using state abstraction
   + Refining and modifying a state abstraction

- Each folder contains a package `Reverser` which contains a procedure to reverse the elements of an array using a stack

-------------
Global Data
-------------

- Folder :filename:`globals` contains an implementation of a state using global data

   + Global objects can be added to subprogram global contracts
   + Not the best use of data hiding!

- :menu:`Prove` (or :menu:`Examine`) the packages

   + No errors because data flow specified via global contracts

      * Actual global objects are used in the contracts (Actual names `Sp` and `Vec`)

      * Even `Reverser` uses the global objects in its contract

- Imagine how much more complicated this would be with more global data

----------------
Abstract State
----------------

- Folder :filename:`state` contains an implementation of a state using `Abstract_State`

   + `A_Stack` defines an abstract name (`The_Stack`) to track internal state

      - No reliance on global data
      - Global data can be moved into package body

- :menu:`Prove` (or :menu:`Examine`) the packages

   + No errors because data flow specified via global contracts

      * No global objects are used in the contracts (We will complete ("refine") the state definition later)

      * Now `Reverser` just uses the abstract state name

------------------
Refining a State
------------------

- Folder :filename:`refined` is an expansion of the `state` example

   + Package body for `A_Stack` defines the content of `The_Stack` using a `Refined_State` contract

      * Specify constituents of the state abstraction `The_Stack`.

- Subprograms previously declared with `Abstract_State` may add `Refined_State` to their implementation

   * Specify global data being referenced

- Lots of "extra work" to add this information

   + Only affects body of implementation
   + If not provided by the user, computed by the tool

- :menu:`Prove` (or :menu:`Examine`) the packages

   + **NOTE:** Preconditions have been added to `A_Stack`

-------------------------------
Adding a Utilization Function
-------------------------------

+ Add a function `Utilization` to return a `Natural` which gives maximum depth of `The_Stack` used so far.

   * Has to be declared in specification of `A_Stack`

      - Do not change any other part of the specification

   * Implementation can be an expression function

      - But must be declared in the body

+ Add a new global object to maintain the maximum utilization
+ Analyzing your code generates an error!

   .. code:: console

      body of package A_Stack has unused hidden states

   - You have modified the state of the package
   - If you did not initialize your object, that will also generate an error

      * Because the spec uses an `Initializes` contract for the state

--------------------------------------
Adding a Utilization Function (cont)
--------------------------------------

+ Now modify the `Refined_State`

   * :menu:`Prove` your code to check the implementation.

+ Modify `Push` to update the maximum utilization object
- :menu:`Prove` your changes

   * If you did not modify the `Refined_Global` for `Push` you will get an error

      - Because now `Push` accesses another part of the state

- You have successfully added new functionality and extra state components

   * Without affecting any existing users of the package or their proofs.
   * This is only achievable through the use of abstraction and encapsulation.

* External user of the state (global contract in `Reverser`) did not need to change!

   * The content may have changed, but not the purpose
   * This may not have been true without abstraction

.. container:: speakernote


   Added global object must appear before first use of refined_state
