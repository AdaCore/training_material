-----------
Usage Lab
-----------

* Test a simplistic stack

   .. code:: Ada

      package Simple_Stack is

         procedure Push (Item : Integer);
         function Pop return Integer;
         function Empty return Boolean;
         function Full return Boolean;
         function Top return Integer;
         function Count return Natural;

         procedure Reset;

      end Simple_Stack;

  * There is a bug in the code - your testing should find it!

* Copy the :filename:`test_020_usage` lab from the course materials location

*Note: Many of the following pages use animation to first give you a task and then show you how to do it.* :menu:`Page Down` *does not always go to the next page!*

----------------
Initialization
----------------

.. container:: animate 1-

   * Build a test harness for the project

.. container:: animate 2-

   * One possible command

      ::

         gnattest -P default.gpr --harness-dir=my_test

      * If you do not specify :command:`--harness-dir=<dir>` the harness goes in :filename:`obj/gnattest/harness`

   * Build and run the test driver

.. container:: animate 3-

   ::

     cd obj/my_test
     gprbuild -P test_driver
     test_runner

   For each subprogram in :ada:`Stack`, you should get a line like

   .. container:: latex_environment tiny

      ``simple_stack.ads:3:4: error: corresponding test FAILED: Test not implemented. (simple_stack-test_data-tests.adb:44)``

   With a summary line like
   
   .. container:: latex_environment tiny

      ``7 tests run: 0 passed; 7 failed; 0 crashed.``

-----------------------
Build Your First Test
-----------------------

* Build a test to prove that :ada:`Push` works

   * Criteria would be that, after the call:

      * :ada:`Empty` should be :ada:`False`
      * :ada:`Count` should be :ada:`1`
      * :ada:`Top` should be whatever was pushed

   * Hint: the filename you're looking for is in the ``Test not implemented`` message

   *Next page for example solutions*

* Build and run the test harness to verify your test passes

.. container:: animate 2-

   ::

      gprbuild -P test_driver.gpr
      test_runner

   *Note indication that test passed*

---------------
Example Tests
---------------

* Solution 1 - one check

   .. code:: Ada

      declare
         Pushed : constant integer := 123;
      begin
         Push (Pushed);
         AUnit.Assertions.Assert ((not Empty) and then Top = Pushed and then Count = 1,
                                  "Push test failed");
      end;

* Solution 2 - multiple checks

   .. code:: Ada

      declare
         Pushed : constant integer := 123;
      begin
         Push (Pushed);
         AUnit.Assertions.Assert ( not Empty,
                                   "Test failed - stack empty");
         AUnit.Assertions.Assert ( Top = Pushed,
                                   "Test failed - Top /= pushed value");
         AUnit.Assertions.Assert ( Count = 1,
                                   "Test failed - count incorrect");
      end;

*Note that when multiple assertions are used, the test stops on the first failed assertion*

--------------------
Improve First Test
--------------------

* We want to know what happens when :ada:`Push` pushes to a full stack

* Add a second part of the testcase to test this

   * :ada:`Push` inside a loop is easiest

.. container:: animate 2-

   .. code:: Ada

      while not Full loop
         Push (234);
      end loop;
      Push (345);
      AUnit.Assertions.Assert (Full and then Top = 234,
                               "Push to a full stack failed");

----------------------------
Test Remaining Subprograms
----------------------------

* Test all remaining subprograms

   * Criteria should be based on what **should** happen

      * Not what **does** happen

* Remember - there is a bug in the code!

   * If a test fails - recheck your assertions
   * If your assertions are correct - then check the code
   * Feel free to fix the code or leave the failure

      * Both are common practices

.. container:: animate 2-

   *Hint: Only one execution, so global state is remembered*

.. container:: animate 3-

   **Call** :ada:`Reset` **to reset the stack data**

----------------
Sample Answers
----------------

These answers assume the bug in the code is fixed

.. container:: animate 2-

   Bug is in :ada:`Pop` - should be

   .. code:: Ada

      function Pop return Integer is
      begin
         if not Empty then
            Next_Available := Next_Available - 1;
         end if;
         return Stack (Next_Available);
      end Pop;

Answers on next pages

---------------
Answers (1/2)
---------------

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

---------------
Answers (2/2)
---------------

.. code:: Ada

   -- Empty
   Reset;
   AUnit.Assertions.Assert (Empty, "Stack not empty");

   -- Full
   while not Full loop
      Push (567);
   end loop;
   Push (999);
   AUnit.Assertions.Assert (Full and then Top = 567,
                            "Full check failed");

   -- Top
   Reset;
   declare
      Pushed : constant integer := 234;
   begin
      Push (Pushed);
      AUnit.Assertions.Assert (Pushed = Top,
                               "Top test failed");
   end;

   -- Count
   Reset;
   Push (111);
   AUnit.Assertions.Assert (Count = 1,
                            "Count test failed");

   -- Reset
   Reset;
   AUnit.Assertions.Assert (Count = 0 and then Empty,
                            "Reset test failed");



