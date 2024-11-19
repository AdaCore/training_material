--------------------------
Controlling GNATtest Lab
--------------------------

* We are going to use the same code as the previous lab

   * But clean up our test code
   * And try some :toolname:`GNATtest` switches

* Copy the :filename:`test_030_controlling_gnattest` lab from the course materials location

   * Put it in a new directory so you can refer back to the *Usage Lab* answers

*Note: Many of the following pages use animation to first give you a task and then show you how to do it.* :menu:`Page Down` *does not always go to the next page!*

----------------------------
Build Harness for One Unit
----------------------------

.. container:: animate 1-

   * Build a test harness only for the :ada:`Simple_Stack` unit

.. container:: animate 2-

   ::

      gnattest -P default.gpr --harness-dir=my_test -U simple_stack.ads

   *Note the unit specifier is a filename, not Ada name. (Also, spec or body filename is allowed)*

   * Run all the tests to get the *not implemented* message

.. container:: animate 3-

   ::

     cd obj/my_test
     gprbuild -P test_driver
     test_runner
     ...
     7 tests run: 0 passed; 7 failed; 0 crashed.

   * Now run the tests with *not implemented* tests indicating *passed*

.. container:: animate 4-

   ::

     cd obj/my_test
     gprbuild -P test_driver
     test_runner --skeleton-default=pass
     ...
     7 tests run: 7 passed; 0 failed; 0 crashed.

--------------
Create Tests
--------------

* Re-write or copy the test answers from the *Usage* lab (here are some examples)

   .. code:: Ada

      -- Push
      Reset;
      declare
         Pushed : constant integer := 123;
      begin
         Push (Pushed);
         AUnit.Assertions.Assert ((not Empty) and then Top = Pushed and then Count = 1,
                                  "Push test failed");
      end;

      while not Full loop
         Push (234);
      end loop;
      Push (345);
      AUnit.Assertions.Assert (Full and then Top = 234,
                               "Push to a full stack failed");

      -- Pop
      Reset;
      declare
         Pushed : constant integer := 234;
	    Popped : integer;
      begin
         Push (Pushed);
	    Popped := Pop;
         AUnit.Assertions.Assert (Pushed = Popped and then Empty and then Count = 0,
                                  "Pop test failed");
      end;

      -- Empty
      Reset;
      AUnit.Assertions.Assert (Empty, "Stack not empty");

-----------------------------------
Ensure Every Test Starts the Same
-----------------------------------

.. container:: animate 1-

   * Previously, every test called :ada:`Simple_Stack.Reset` to ensure the stack was initialized

      * Lots of redundant code

   * Remove calls to :ada:`Simple_Stack.Reset` and (re)run the tests

.. container:: animate 2-

   * Answer

      ::

        cd obj/my_test
        gprbuild -P test_driver
        test_runner
        ...
        7 tests run: 2 passed; 5 failed; 0 crashed.

   * Rerun the tests but do not display the passed tests

.. container:: animate 3-

   * Answer

      ::

        test_runner --passed-tests=hide
        ...
        7 tests run: 2 passed; 5 failed; 0 crashed.

      *Status is the same, we just do not see individual passed tests*

-------------------
Add "Global" Code
-------------------

.. container:: animate 1-

   * Add code to call :ada:`Simple_Stack.Reset` before every test case

.. container:: animate 2-

   :filename:`simple_stack-test_data.adb`

   .. code:: Ada

      procedure Set_Up (Gnattest_T : in out Test) is
         pragma Unreferenced (Gnattest_T);
      begin
         Reset;
      end Set_Up;

   * For extra credit, add code to clear global data

.. container:: animate 3-

   :filename:`simple_stack-test_data.adb`

   .. code:: Ada

      procedure Tear_Down (Gnattest_T : in out Test) is
         pragma Unreferenced (Gnattest_T);
      begin
         Reset;
      end Tear_Down;

   *This ensures the stack is reset when tests for other units are run*
