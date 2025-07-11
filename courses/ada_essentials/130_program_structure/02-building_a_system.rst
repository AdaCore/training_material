===================
Building a System
===================

-------------------
What Is a System?
-------------------

* Also called Application or Program or ...
* Collection of :dfn:`library units`

   - Which are a collection of packages or subprograms

---------------
Library Units 
---------------

* Those units not nested within another program unit
* Candidates

   - Subprograms
   - Packages
   - Generic Units
   - Generic Instantiations
   - Renamings

* Dependencies between library units via :ada:`with` clauses

   - What happens when two units need to depend on each other?

