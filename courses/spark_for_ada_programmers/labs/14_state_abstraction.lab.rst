-----------------------
State Abstraction Lab
-----------------------

- Find the :filename:`14_state_abstraction` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

----------------
Abstract State
----------------

- Define an abstract state called :ada:`The_State` to hold all of the state of
  package :ada:`Basics`

- Move all the state of package :ada:`Basics` into its private part with
  suitable aspects :ada:`Part_Of`

- Define the state refinement in the package body

- Run :toolname:`GNATprove` in flow analysis mode

----------------------
Dependency Contracts
----------------------

- Update the data dependency and flow dependency contracts to use :ada:`The_State`

- Run :toolname:`GNATprove` in flow analysis mode

  + There should be no check messages, only a warning: :command:`no procedure
    exists that can initialize abstract state`

- Add a procedure :ada:`Init_The_State` that initializes all of the state

  + The body of this procedure can simply call :ada:`Init_The_Rec` and
    :ada:`Init_The_Table`
  + Do you understand how :toolname:`GNATprove` checks that this is correct?
