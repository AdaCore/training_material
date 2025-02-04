==============
Introduction
==============

-------------------------
Constructor / Destructor
-------------------------

* Possible to specify behavior of object initialization, finalization, and assignment

   - Based on type definition
   - Type must derive from `Controlled` or `Limited_Controlled` in package `Ada.Finalization`

* This derived type is called a *controlled type*

    - User may override any or all subprograms in `Ada.Finalization`
    - Default implementation is a null body

