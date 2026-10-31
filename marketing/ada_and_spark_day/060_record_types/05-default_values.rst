================
Default Values
================

--------------------------
Component Default Values
--------------------------

.. code:: Ada

   type Complex is
     record
       Real : Float := 0.0;
       Imaginary : Float := 0.0;
     end record;
   -- all components use defaults
   Phasor : Complex;
   -- all components must be specified
   I : constant Complex := (0.0, 1.0);

-----------------------------------
Defaults Within Record Aggregates
-----------------------------------

* Specified via the :dfn:`box` notation
* Value for the component is thus taken as for a stand-alone object declaration

   - So there may or may not be a defined default!

* Can only be used with "named association" form

   - But can mix forms, unlike array aggregates

.. code:: Ada

   type Complex is
     record
       Real : Float := 0.0;
       Imaginary : Float := 0.0;
     end record;
   Phase := (42.0, Imaginary => <>);
