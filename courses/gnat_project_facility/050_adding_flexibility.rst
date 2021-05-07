********************
Adding Flexibility
********************

.. role:: ada(code)
   :language: ada

======================================
Variables for Conditional Processing
======================================

---------------------------------------------------
Two Sample Projects for Different Switch Settings
---------------------------------------------------

.. container:: latex_environment scriptsize

 .. columns::

   .. column::

      .. code:: Ada

         project Debug is 
           for Object_Dir use "debug"; 
           package Builder is
             for Default_Switches ("Ada")
                use ("-g"); 
           end Builder; 
           package Compiler is
             for Default_Switches ("Ada") 
                use ("-fstack-check",
                     "-gnata",
                     "-gnato"); 
           end Compiler;
         end Debug; 

   .. column::

      .. code:: Ada

         project Release is
           for Object_Dir use "release";
           package Compiler is 
             for Default_Switches ("Ada")
                use ("-O2"); 
           end Compiler;
         end Release; 

-------------------------------------
External and Conditional References
-------------------------------------

+ Allow project file content to depend on value of environment variables & command-line arguments
+ Reference to external values is by function

  + :ada:`external( name [, default] )` returns value of name as supplied on the command line or as environment variable
  + If name is undefined, return default (if supplied) or ""

+ Set via command line switch (for example)

  *gnatmake -P... - Xname =value ...*

  .. container:: latex_environment footnotesize

     :command:`gnatmake -P/common/build.gpr -Xtarget =test /common/main.adb`

+ **Note:** Command line values take precedence over environment variables

----------------------------------------
External/Conditional Reference Example
----------------------------------------

.. code:: Ada

   project Build is
      type Targets is ("release", "test");
      Target : Targets := external("target", "test");
      case Target is -- project attributes
         when "release" =>
            for Object_Dir use "release";  --
            for Exec_Dir use ".";          --
         when "test" =>
            for Object_Dir use "debug";    --
      end case;
      package Compiler is
         case Target is
            when "release" =>
               for Default_Switches ("Ada") use ("-O2");          --
            when "test" =>
               for Default_Switches ("Ada") use                   --
                     ("-g", "-fstack-check", "-gnata", "-gnato"); --
         end case;
      end Compiler;
      ...
   end Build;

--------------------------------------------
Scenario Controlling Source File Selection
--------------------------------------------

.. code:: Ada

   project Demo is
      ...
      type Displays is ("Win32", "ANSI");
      Output : Displays := external ("OUTPUT", "Win32");
      ...
      package Naming is
         case Output is
            when "Win32" =>
               for Body ("Console") use "console_win32.adb";
            when "ANSI" =>
               for Body ("Console") use "console_ansi.adb";
           end case;
      end Naming;
   end Demo;

* Source Files

 .. list-table::
   :header-rows: 1
    
   * - :filename:`console.ads`

     - 
     - :filename:`console_win32.adb`
     - 
     - :filename:`console_ansi.adb`

   * - :ada:`package Console is`

     - 
     - :ada:`package body Console is`
     - 
     - :ada:`package body Console is`

   * - :ada:`...`

     - 
     - :ada:`...`
     - 
     - :ada:`...`

   * - :ada:`end Console;`

     - 
     - :ada:`end Console;`
     - 
     - :ada:`end Console;`
