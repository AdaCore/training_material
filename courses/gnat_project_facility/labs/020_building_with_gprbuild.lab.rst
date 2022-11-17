--------------
Start GPRBuild
--------------

* Open a command shell
* Go to :filename:`020_building_with_gprbuild` directory (under :filename:`source`)

  * Contains a main procedure and a supporting package for the "8 Queens" problem

* Use an editor to create minimum project file

  * Name the project anything you wish
  * Filename and project name should be the same

* Build :ada:`Queens` using :command:`gprbuild` and the project file as-is

  * Use :command:`-P` argument on the command line to specify project file
  * Must also specify file name on command line to get executable

    * For example: :command:`gprbuild -P lab.gpr queens`

* Clean the project with :command:`gprclean`

  * Use :command:`-P` argument on the command line to specify project file
  * Note that the :filename:`queens.exe` executable remains

    * Plus (possibly) some intermediate files

--------------------------------
GPRbuild Lab - Simple GPR File
--------------------------------

.. code:: Ada

   project Lab is
   end Lab;

:command:`gprbuild -P lab.gpr`
   Only compiles source files

:command:`gprbuild -P lab.gpr queens`
   Compiles source and creates :filename:`queens` executable

:command:`gprclean -P lab.gpr`
   Deletes ALI and object files for :ada:`Queens` and :ada:`Queens_Pkg`

---------------------
GPRbuild Lab Part 2
---------------------

* Change project file so that it specifies the main program
* Build again, without specifying the main on the command line

  * Use only :command:`-P` argument on the command line to specify project file

* Clean the project with :command:`gprclean` again

  * Note the :filename:`queens` executable is now also deleted (as well as any intermediate files)

---------------------------------------
GPRbuild Lab - Main Program Specified
---------------------------------------

.. code:: Ada

  project Lab is
     for Main use ( "main.adb" );
  end Lab;

:command:`gprbuild -P lab.gpr`
   Compiles source and creates :filename:`queens` executable

:command:`gprclean -P lab.gpr`
   Deletes all generated files

