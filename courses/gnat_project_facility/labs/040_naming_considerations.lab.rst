--------------------------------
Source File Naming Schemes Lab
--------------------------------

* Open a command shell
* Go to the :filename:`filenames` folder under :filename:`projects`
* Copy the project file from the first lab exercise
* Copy the file :filename:`fibonacci.adb` here too
* Modify the project file so that it works with Rational Apex source file names

   * Apex uses :filename:`.1.ada` for specs and :filename:`.2.ada` for bodies
   * Hint 1: You've seen one very similar
   * Hint 2: Don't use the :ada:`Source_Files` attribute

* Rename :filename:`fibonacci.adb` to use Apex name format
* Build the main using the project

-----------------------
Overview Lab Solution
-----------------------

.. code:: Ada

   project Lab is
      for Main use ("fibonacci");
      package Naming is
         for Casing use "lowercase";
         for Dot_Replacement use ".";
         for Spec_Suffix ("Ada") use ".1.ada";
         for Body_Suffix ("Ada") use ".2.ada";
      end Naming;
   end Lab;
