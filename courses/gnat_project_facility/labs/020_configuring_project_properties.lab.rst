------------------------------------
Configuring Project Properties Lab
------------------------------------

* Still in :filename:`fibonacci` directory (under :filename:`source`)
* Modify project file to disable overflow checking

   * Add the :ada:`Compiler` package
   * Insert :ada:`Default_Switches` attribute for Ada in :ada:`Compiler` package
   * Set switch :command:`-gnato0` in the attribute

      * Disable overflow checking

* Build again

   * Need to use switch :command:`-f` on command line to force rebuild
   * (Changes to GPR file do not automatically force recompile)

---------------------------------------------
Configuring Project Properties Lab Solution
---------------------------------------------

.. code:: Ada

   project Lab is
      for Main use ("main.adb");
      package Compiler is
         for Default_Switches ("Ada") use ("-gnato0");
      end Compiler;
   end Lab;
