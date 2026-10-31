==============
Generic Data
==============

-------------
Definitions
-------------

* A formal generic parameter is a template
* Properties are either :dfn:`constraints` or :dfn:`capabilities`

    - Expressed from the **body** point of view
    - Constraints: e.g. unconstrained, :ada:`limited`
    - Capabilities: e.g. :ada:`tagged`, primitives

.. code:: Ada

   generic
      type Pv is private;           -- allocation, copy, assignment, "="
      with procedure Sort (T : Pv); -- primitive of Pv
      type Unc (<>) is private;     -- allocation require a value
      type Lim is limited private;  -- no copy or comparison
      type Disc is (<>);            -- 'First, ordering
   package Generic_Pkg is [...]

* Actual parameter **may** require constraints, and **must** provide capabilities

.. code:: Ada

   package Pkg is new Generic_Pkg (
      Pv => Integer, -- has capabilities of private
      Sort => Sort -- procedure Sort (T : Integer)
      Unc => String,  -- uses "unconstrained" constraint
      Lim => Float,   -- does not use "limited" constraint
      Disc => Boolean, -- has capability of discrete
  );
