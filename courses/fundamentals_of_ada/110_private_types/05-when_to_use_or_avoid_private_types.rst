====================================
When to Use or Avoid Private Types
====================================

---------------------------
When to Use Private Types
---------------------------

* Implementation may change

   - Allows users to be unaffected by changes in representation

* Normally available operations do not "make sense"

   - Normally available based upon type's representation
   - Determined by intent of ADT

   .. code:: Ada

      A : Valve;
      B : Valve;
      C : Valve;
      ...
      C := A + B;  -- addition not meaningful

* Users have no "need to know"

   - Based upon expected usage

-----------------------------
When to Avoid Private Types
-----------------------------

* If the abstraction is too simple to justify the effort

   - But that's the thinking that led to Y2K rework

* If normal user interface requires representation-specific operations that cannot be provided

   - Those that cannot be redefined by programmers
   - Would otherwise be hidden by a private type
   - If `Vector` is private, indexing of components is annoying

      .. code:: Ada

        type Vector is array (Positive range <>) of Float;
        V : Vector (1 .. 3);
        ...
        V (1) := Alpha; -- Illegal since Vector is private

