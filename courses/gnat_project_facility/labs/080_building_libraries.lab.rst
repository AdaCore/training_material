------------------------
Building Libraries Lab
------------------------

* Open a command shell
* Go to the :filename:`library` subdirectory
* Create a library for the sources in the :filename:`amazing` project's :filename:`utils` directory
* Use the :filename:`libs` directory to hold the library
* Use the :filename:`objs` directory to hold the objects
* Create the project file in this :filename:`library` subdir

   * Hint: the :ada:`Source_Dirs` attribute will be very useful

* Build the library

---------------------------------
Building Libraries Lab Solution
---------------------------------

.. code:: Ada

   library project Utils_Lib is
      for Library_Name use "utils";
      for Library_Dir use "libs";
      for Object_Dir use "objs";
      for Source_Dirs use ("../amazing/source/utils/");
   end Utils_Lib;


