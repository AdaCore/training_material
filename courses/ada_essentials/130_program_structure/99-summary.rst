=========
Summary
=========

---------
Summary
---------

* Hierarchical library units address important issues

   - Direct support for subsystems
   - Extension without recompilation
   - Separation of concerns with controlled sharing of visibility

* Parents should document assumptions for children

   - "These must always be in ascending order!"

* Children cannot misbehave unless imported ("with'ed")

* Not uncommon for two package specs to be interdependent

  * :ada:`limited with` can resolve circularity
  * May involve rethinking your type definitions
