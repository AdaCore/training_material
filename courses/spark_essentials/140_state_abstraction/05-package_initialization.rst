========================
Package Initialization
========================

--------------------------------
Data Dependencies of a Package
--------------------------------

* The :dfn:`package elaboration` executes code

  - For all declarations in the package spec
  - For all declarations in the package body
  - And the statements at the end of the package body

* Only package state can be written during package elaboration

  - A package cannot write the state of another package in SPARK

* Aspect :ada:`Initializes` specifies state initialized during elaboration

  - If present, must be complete, including visible and hidden state
  - Otherwise, :toolname:`GNATprove` generates it
  - Similar to the outputs of mode :ada:`Output` for the package elaboration

.. code:: Ada

   package Stack with
      Abstract_State => The_Stack,
      Initializes    => The_Stack
   is
      -- Flow analysis verifies that Top and Content are
      -- initialized at package elaboration.

--------------------------------
Flow Dependencies of a Package
--------------------------------

* Initialization of package state can depend on other packages

  - This dependency needs to be specified in aspect :ada:`Initializes`
  - If no such aspect, :toolname:`GNATprove` also generates these dependencies
  - Similar to the :ada:`Depends` aspect for the package elaboration

.. code:: Ada

   package P with
      Initializes => (V1, V2 => External_Variable)
   is
      V1 : Integer := 0;
      V2 : Integer := External_Variable;
   end P;
   -- The association for V1 is omitted, it does not
   -- depend on any external state.

