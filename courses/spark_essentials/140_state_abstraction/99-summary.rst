=========
Summary
=========

-------------------
State Abstraction
-------------------

* Abstract state represents hidden state of a package

  - Variables in the private part or body
  - Visible state of nested packages (variables and abstract states)
  - Visible state of private child packages
  - Constants with variable input

* Each abstract state must be refined into constituents

  - Annotation :ada:`Part_Of` needed on declarations in the private part

* Dependency contracts use abstract states to refer to hidden state

* Initialization at elaboration specified with aspect :ada:`Initializes`

  - This concerns both visible and hidden state
  - This replaces aspects :ada:`Global` and :ada:`Depends` for package
    elaboration
