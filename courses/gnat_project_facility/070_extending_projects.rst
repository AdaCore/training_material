********************
Extending Projects
********************

.. role:: ada(code)
   :language: ada

====================
Extending Projects
====================

--------------------
Extending Projects
--------------------

+ Allows using modified versions of source files without changing the original sources
+ Based on "inheritance" of parent project's properties

  + Source files
  + Switch settings

+ Supports localized build decisions and properties

  + Inherited properties may be overridden with new versions

+ Hierarchies permitted

-------------------------------
Limites on Extending Projects
-------------------------------

+ A project that extends/modifies a project can also import other projects.
+ Can't import both a parent and a modified project.
+ Can extend only one other project at a time.

------------------------------------------
Multiple Versions of Unit Bodies Example
------------------------------------------

+ Assume *baseline* directory structure:

   + :filename:`baseline.gpr`
   + :filename:`filename.ads`
   + :filename:`filename.adb`
   + :filename:`application.adb`

+ For testing, you want to

   + Replace :filename:`filename.adb` with a dummy version
   + Use :filename:`test_driver.adb` as the main program

----------------------------------------
Multiple Versions of Unit Bodies Files
----------------------------------------

* *Baseline* will have a GPR file like:

   .. code:: Ada

      project Baseline is
         for Source_Dirs use ( "src" );
         for Main use ( "application.adb" );
      end Baseline;

* The test GPR file would look like:

   .. code:: Ada

      project Test_Baseline extends "baseline" is
         for Source_Dirs use ( "test_code" );
         for Main use ( "test_driver.adb" );
      end Test_Baseline;

====================
Aggregate Projects
====================

--------------------
Aggregate Projects
--------------------

+ Represent multiple, related projects

  + Related especially by common source code

+ Allow managing options in a centralized way
+ Compilation optimized for sources common to multiple projects

  + Doesn't compile more than necessary

---------------------------
Aggregate Project Example
---------------------------

.. code:: Ada

   aggregate project Agg is
      for Project_Files use ("A.gpr", "B.gpr"); 
      for Project_Path use ("../dir1", "../dir1/dir2"); 
      for external ("BUILD") use "PRODUCTION"; 

      package Builder is 
         for Global_Compilation_Switches ("Ada")
               use ("-O1", "-g");
      end Builder; 
   end Agg;

=====
Lab
=====

.. include:: labs/070_extending_projects.lab.rst
