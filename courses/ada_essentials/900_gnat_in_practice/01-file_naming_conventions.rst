=========================
File Naming Conventions
=========================

---------------------------------
Default File Naming Conventions
---------------------------------

.. container:: columns

  .. container:: column

    * GNAT compiler assumes one compilable entity per file

      * Package specification, subprogram body, etc

    * Filenames should match the name of the compilable entity

      * Replacing **.** with **-**
      * File extension is :filename:`.ads` for specifications and :filename:`.adb` for bodies

  .. container:: column

    .. container:: latex_environment tiny

      :filename:`some_types.ads`

      .. code:: Ada

         package Some_Types is
            type One_T is new Integer;
            type Two_T is new Character;
            function Convert
              (Src : One_T)
               return Two_T;
         end Some_Types;

      :color-white:`blank line`

      :filename:`some_types.adb`

      .. code:: Ada

         package body Some_Types is
            function Convert
                (Src : One_T)
                 return Two_T is
              (Two_T'Val (Integer (Src)));
         end Some_Types;

      :color-white:`blank line`

      :filename:`some_types-child.adb`

      .. code:: Ada

         function Some_Types.Child
           (Src : Two_T)
            return One_T is
         begin
            return One_T (Two_T'Pos (Src));
         end Some_Types.Child;

---------------------------------------
Converting to GNAT Naming Conventions
---------------------------------------

* Use :toolname:`gnatchop` to convert file containing Ada code to GNAT names

   * If file contains multiple units, will generate multiple files
   
   * :command:`-w` is the most common switch - will overwrite existing files

   * Can specify destinaton directory

      * If not specified, files created in same directory are source

* Files for standard library units created using :command:`-k` switch

   * **krunch** - generated filename has no more than 8 characters
   * :ada:`Ada.Characters` |rightarrow| :filename:`a-charac.ads`
   * Historical reasons (i.e "8.3" filenames)

--------------------------------
Using Other Naming Conventions
--------------------------------

* Sometimes you don't want to change filenames

   * Sharing source across multiple compilers
   * Different versions of a file based on build parameters

* Controlled via package :ada:`Naming` in project file

   * Example: your source files use :filename:`.1.ada` for specs and :filename:`.2.ada` for bodies

      .. code:: Ada

         package Naming is
            for Spec_Suffix ("Ada") use ".1.ada";
            for Body_Suffix ("Ada") use ".2.ada";
         end Naming;

   * Example: different implementations for different platforms
      
      .. code:: Ada

         package Naming is
            case Platform is
               when "windows =>
                  for Body ("My_IO") use "my_io.windows";
               when "linux" =>
                  for Body ("My_IO") use "my_io.linux";
         end Naming;

------------------
More Information
------------------

For further information, see Section 3.3 *File Naming Topics and Utilities* in the **GNAT User's Guide**
