--------------------------------
Source File Naming Schemes Lab
--------------------------------

* Open a command shell
* Go to the :filename:`naming` folder under :filename:`projects`
* Modify the project file so that it works with Rational Apex source file names

   * Apex uses :filename:`.1.ada` for specs and :filename:`.2.ada` for bodies

* Build the executable

-----------------------
Overview Lab Solution
-----------------------

.. code:: Ada

   project Lab is
      for Main use ("main.2.ada");
      package Naming is
         for Casing use "lowercase";
         for Dot_Replacement use ".";
         for Spec_Suffix ("Ada") use ".1.ada";
         for Body_Suffix ("Ada") use ".2.ada";
      end Naming;
   end Lab;

* Could have used :ada:`for Spec ("Fibonacci") use "fibonacci.1.ada";`

  * Works for rare instances, harder for lots of instances
