=========
Summary
=========

---------
Summary
---------

* :ada:`'Class` attribute

   - Allows subprograms to be used for multiple versions of a type

* Dispatching

   - Abstract types require concrete versions
   - Abstract subprograms allow template definitions

      + Need an implementation for each abstract type referenced

* Runtime call dispatch vs compile-time call dispatching

   - Compiler resolves appropriate call where it can
   - Runtime resolves appropriate call where it can
   - If not resolved, exception
