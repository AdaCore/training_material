=========================
File Naming Conventions
=========================

---------------------------------
Default File Naming Conventions
---------------------------------

* GNAT compiler assumes one compilable entity per file

   * Package specification, subprogram body, etc

* File names should match the name of the compilable entity

   * Replace **.** with **-**

* File extensions describe the usage

   * **.ads** |rightarrow| Specification / Interface
   * **.adb** |rightarrow| Body / Implementation

---------------------------------
Example Filenames with Contents
---------------------------------

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada

         package Some_Types is
            type One_T is new Integer;
            type Two_T is new Character;
            function Convert (Src : One_T)
                              return Two_T;
         end Some_Types;

  .. container:: column

    .. container:: latex_environment tiny

      *Package specification for* :ada:`Some_Types` *is in file* :filename:`some_types.ads`

.. container:: latex_environment tiny

   :color-white:`blank line`

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada

         package body Some_Types is
            function Convert (Src : One_T)
                              return Two_T is
               (Two_T'Val (Integer (Src)));
         end Some_Types;

  .. container:: column

    .. container:: latex_environment tiny

      *Package body for* :ada:`Some_Types` *is in file* :filename:`some_types.adb`

.. container:: latex_environment tiny

   :color-white:`blank line`

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada

         function Some_Types.Child (Src : Two_T)
                                    return One_T;

  .. container:: column

    .. container:: latex_environment tiny

      *Subprogram specification for function* :ada:`Child` *which is a child of* :ada:`Some_Types` *is in file* :filename:`some_types-child.ads`

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

   * **Example:** your source files use :filename:`.1.ada` for specs and :filename:`.2.ada` for bodies

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
