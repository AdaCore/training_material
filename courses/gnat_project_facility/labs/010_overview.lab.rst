--------------
Overview Lab
--------------

   * Open a command shell
   * Go to :filename:`fibonacci` directory (under :filename:`materials`)

      * Contains a main procedure and a supporting package

   * Use an editor to create minimum project file

      * Name the project anything you wish

   * Build fibonacci using gprbuild and the project file as-is

      * Use :command:`-P` argument on the command line to specify project file
      * Must also specify file name on command line to get executable

   * Clean the project with gprclean

      * Use :command:`-P` argument on the command line to specify project file
      * Note that the :filename:`main` executable remains

   * Change project file so that it specifies the main program

   * Build again, without specifying the main on the command line

      * Use only :command:`-P` argument on the command line to specify project file

   * Clean the project with gprclean again

      * Note the :filename:`main` executable is now also deleted

-----------------------
Overview Lab Solution
-----------------------

* Minimal Project File

   .. code:: Ada

      project Lab is
      end Lab;

* Minimal Project File with main specified

   .. code:: Ada

      project Lab is
         for Main use ( "main.adb" );
      end Lab;
