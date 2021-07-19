----------------------------------
Structuring Your Application Lab
----------------------------------

.. columns::

   .. column::

      * The **Amazing** Project

         * A multi-threaded maze solver
         * Current version of :filename:`amazing.gpr`

            * Puts all :filename:`obj` and :filename:`ali` files in :filename:`objs`
            * Puts executable in project root
            * Has hardcoded switches (no debug/release scenario)

   .. column::

      .. image:: ../../images/gpr_amazing_lab.jpg

------------------------
Original "amazing.gpr"
------------------------

.. code:: Ada

   project Amazing is

      for Languages use ("Ada");
      for Main use ("mice.adb", "mouse.adb");

      for Object_Dir use "objs";
      for Exec_Dir use ".";
      for Source_Dirs use ("source", "source/consoles", "source/utils");

      type Displays is ("Win32", "ANSI");
      Output : Displays := external ("OUTPUT", "Win32");

      package Builder is
         for Default_Switches ("ada") use ("-gnatQ");
      end Builder;

      package Compiler is
         for Default_Switches ("ada") use ("-O2", "-gnatwa");
      end Compiler;

      package Naming is
         case Output is
            when "Win32" =>
               for Implementation ("console") use "console_win32.adb";
                  when "ANSI" =>
                     for Implementation ("console") use "console_ansi.adb";
            end case;
      end Naming;

   end Amazing;

---------------------
Assignment Part One
---------------------

* You will modify the project:

      * Put all :filename:`obj` and :filename:`ali` files in either :filename:`objs/debug` or :filename:`objs/release`
      * Put executable in either :filename:`bin/debug` or :filename:`bin/release`
      * Use different compiler and builder switches for :command:`debug`, :command:`release`, and :command:`release_optimized` scenarios
      * Use a scenario variable to control all this behavior

* To do so, edit the :filename:`amazing.gpr` file to apply project :filename:`../common/build_config.gpr`

      * Defines the scenario variable
      * Defines compiler switches
      * Defines builder switches
      * Don't do it all in :filename:`amazing.gpr`, use the additional gpr file!

* And make any other changes necessary

---------------------
Assignment Part Two
---------------------

* Build the project with no additional args

   * Specify the Console scenario variable if not using Windows

   :command:`gprbuild -P amazing`

* Observe 

   * Where executable and :filename:`obj/ali` files go
   * What switches are applied

* Build the project in the *release* scenario

   :command:`gprbuild -P amazing -XBUILD=release`

* Observe 

   * Where executable and :filename:`obj/ali` files go
   * What switches are applied

* Run the program

   * For example: :command:`mice -w 10 -h 10`
   * Can just enter :command:`mice` to get help

-------------------------------
Solution - Configuration File
-------------------------------

.. code:: Ada

   project Build_Config is

      type Build_Type is ("debug", "release", "release_optimized");
      Build : Build_Type := external ("BUILD", "debug");
      for Source_Dirs use ();

      package Compiler is
         case Build is
            when "debug" =>
               for Default_Switches ("Ada") use ("-O0", "-g");
            when "release" =>
               for Default_Switches ("Ada") use ("-O1");
            when "release_optimized" =>
               for Default_Switches ("Ada") use ("-O2");
         end case;
      end Compiler;

      package Builder is
         case Build is
            when "debug" =>
               for Default_Switches ("Ada") use ("-g");
            when others =>
               for Default_Switches ("Ada") use ();
         end case;
      end Builder;

   end Build_Config;

----------------------
Solution - Main File
----------------------

.. code:: Ada

   with "../common/build_config.gpr";
   project Amazing is

      for Languages use ("Ada");
      for Main use ("mouse.adb", "mice.adb");
      for Source_Dirs use ("source", "source/consoles", "source/utils");

      case Build_Config.Build is
         when "debug" =>
            for Exec_Dir use "bin/debug";
            for Object_Dir use "objs/debug";
         when others =>
            for Exec_Dir use "bin/release";
            for Object_Dir use "objs/release";
      end case;

      type Displays is ("Win32", "ANSI");
      Output: Displays := external ("OUTPUT", "ANSI");

      package Builder renames Build_Config.Builder;

      package Compiler renames Build_Config.Compiler;

      package Naming is
         ... as before
      end Naming;

   end Amazing;
