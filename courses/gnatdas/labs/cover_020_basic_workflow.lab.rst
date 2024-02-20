--------------------
Basic Workflow Lab
--------------------

* We are going to get 100% Statement Coverage on the example from the module

* Copy the :filename:`cover_020_basic_workflow` lab from the course materials location

* Contents of the folder:

  * :filename:`default.gpr` - project file
  * :filename:`src` - source directory

*Note: Many of the following pages use animation to first give you a task and then show you how to do it.* :menu:`Page Down` *does not always go to the next page!*

----------------------------
Preparing the Command Line
----------------------------

1. Open a command prompt window and navigate to the directory containing :filename:`default.gpr`

2. Type :command:`gprbuild -Pdefault.gpr` and press :menu:`Enter` to verify tool is on your path and you can build an executable

   * You can even run the executable (``obj\test_driver.exe`` on Windows or ``obj/test_driver`` on Linux ) to see what happens when you run :ada:`Increment`

3. Prepare the coverage libraries

   * Windows

      ::

         gnatcov setup --prefix=.\gnatcov-rts

   * Linux

      ::

         gnatcov setup --prefix=./gnatcov-rts

   * This creates the :filename:`gnatcov-rts` folder containing the coverage libraries

4. Add the coverage project to the ``GPR_PROJECT_PATH`` environment variable

   .. container:: latex_environment small

      * Windows

         ::

            set GPR_PROJECT_PATH=%GPR_PROJECT_PATH%;\path\to\gnatcov-rts\share\gpr

      * Linux (bash)

         ::

            export GPR_PROJECT_PATH=$GPR_PROJECT_PATH:/path/to/gnatcov-rts/share/gpr

--------------------------------
Instrument, Build, and Execute
--------------------------------

.. container:: animate 1-

   1. Perform statement instrumentation on your source code

.. container:: animate 2-

   ::

      gnatcov instrument -Pdefault.gpr --level=stmt

   2. Then build the instrumented executable

.. container:: animate 3-

   ::

      gprbuild -f -p -Pdefault.gpr --src-subdirs=gnatcov-instr
         --implicit-with=gnatcov_rts.gpr

   3. And then run it

.. container:: animate 4-

   * Windows

      ::

         obj\test_driver.exe

   * Linux  

      ::

         obj/test_driver

   *If you did this correctly, there should be a* :filename:`*.srctrace` *file*

------------------
Viewing Coverage
------------------

.. container:: animate 1-

   1. Add the coverage information into the project

.. container:: animate 2-

   ::

      gnatcov coverage --level=stmt --annotate=xcov test_driver*.srctrace -Pdefault.gpr

   2. Examine the coverage data for the :ada:`ops` unit by viewing the file :filename:`ops.adb.xcov` in the :filename:`obj` folder

   ::

      33% of 6 lines covered
      33% statement coverage (2 out of 6)

      Coverage level: stmt
         1 .: package body Ops is
         2 .:    procedure Apply
         3 .:      (Op :        Op_Kind;
         4 .:       X  : in out Integer) is
         5 .:    begin
         6 +:       case Op is
         7 .:          when Increment =>
         8 +:             X := X + 1;
         9 .:          when Decrement =>
        10 -:             X := X - 1;
        11 .:          when Double =>
        12 -:             X := X * 2;
        13 .:          when Half =>
        14 -:             X := X / 2;
        15 .:       end case;
        16 .:    exception
        17 .:       when others =>
        18 -:          null;
        19 .:    end Apply;
        20 .: end Ops;

--------------------
Improving Coverage
--------------------

* Two ways of getting more coverage

  1. Modify :ada:`test_driver` to test a different value for :ada:`Ops.Apply` :ada:`Op` parameter

       * When you run the executable to generate coverage, you will get a :filename:`srctrace` file with a different timestamp to analyze

  2. Expand :ada:`test_driver` to test all values for :ada:`Ops.Apply` :ada:`Op` parameter in one execution

       * When you run the executable to generate coverage, you will get a :filename:`srctrace` containing all the coverage information

* Using whichever method you want, get 100% statement coverage

  * One possible solution on next page

-------------------
Possible Solution
-------------------

.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   with Ops;
   procedure Test_Driver is
      procedure Run_One
        (Kind  : Ops.Op_Kind;
         Value : Integer) is
         X : Integer := Value;
      begin
         Ops.Apply (Kind, X);
         Put_Line ("Before:" & Value'Image & " After:" & X'Image);
      end Run_One;
   begin
      for Op in Ops.Op_Kind loop
         Run_One (Op, 4);
      end loop;
      Run_One (Ops.Increment, Integer'Last);
   end Test_Driver;

**Hints**

.. container:: animate 2-

   * Whenever you update your source code, you need to re-instrument your project
   * If you modify your source code, previous :filename:`srctrace` files will be out-of-date, generating a message like:

      ::

         warning: traces for body of test_driver (from test_driver.exe-65ba6772-4f18-65baa1dd.srctrace)
             are inconsistent with the corresponding Source Instrumentation Data

      
