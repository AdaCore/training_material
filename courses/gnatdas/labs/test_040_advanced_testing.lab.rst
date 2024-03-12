----------------------
Advanced Testing Lab
----------------------

* We will test a simplistic sensor read/write capability

   * :ada:`Simple.Read` reads a sensor and determines if the value is in range
   * :ada:`Simple.Write` writes to a sensor and reports if the write failed
   * Error messages are sent to an error logger

* Copy the :filename:`test_040_advanced_testing` lab from the course materials location

*Note: Many of the following pages use animation to first give you a task and then show you how to do it.* :menu:`Page Down` *does not always go to the next page!*

---------------------
Create Test Harness
---------------------

.. container:: animate 1-

   * Build a test harness that enables stubbing and allows each test to be run individually

.. container:: animate 2-

   ``gnattest --stub -P default.gpr --separate-drivers=test --harness-dir=my_test``

.. container:: animate 2-

   * :command:`--stub` enables stubbing
   * :command:`--separate-drivers=test` builds an executable for each test

      * Stubbing always requires separate drivers
      * If not specified, an executable is built for each unit

   * :command:`--harness-dir=my_test` puts the harness code in folder :filename:`my_test`

--------------------------------
Build and Execute Test Harness
--------------------------------

.. container:: animate 1-

   * Built the test harness

      * Hint: This is slightly different than earlier labs due to :command:`separate-drivers`

.. container:: animate 2-

   ::

     cd obj/my_test
     gprbuild -P test_drivers.gpr

   *Note the* **s** *at the end of* **driver**

   * Now execute the test harness

      * Hint: This is quite different than earlier labs!

.. container:: animate 3-

   ::

     gnattest test_drivers.list

   * When running multiple test drivers, pass the list of drivers into :toolname:`GNATtest`

      * :filename:`test_drivers.list` is automatically created in :filename:`my_test`
      * Copy and/or edit it to control what tests get run
      * Unlike the monolithic driver, skipped tests are not reported

---------------------------
Build One Test With Stubs
---------------------------

* Build and run test for :ada:`Simple.Read` that 

   * Receives a good value from :ada:`Sensor.Read`
   * Verifies output parameter :ada:`Value` has the previously set result
   * Verifies output parameter :ada:`Status` is True
   * Hint: one mechanism to set stub return data is in the test case skeleton

.. container:: animate 2-

   * Test code inserted into :filename:`simple-test_data-tests.adb`

      .. code:: Ada 

         declare
            Sensor_Value : constant := 12;
            Result : Integer := 0;
            Status : Boolean;
         begin
            Sensor.Stub_Data.Set_Stub_Read_cac9ed_9101fc(Read_Result => Sensor_Value);
            Read (Sensor.Speed, Result, Status);
            AUnit.Assertions.Assert
              (Result = Sensor_Value and then Status,
               "Read positive test failed");
         end;

   * Execution command

      ::

        gnattest test_drivers.list

--------------------------
Build More Advanced Test
--------------------------

* We want to test :ada:`Simple.Read` creates an error message. Criteria would be that it

   * Receives a bad value from :ada:`Sensor.Read`
   * Verifies output parameter :ada:`Status` is False
   * :ada:`Logger.Log_Error` receives the appropriate message
   * Hint: You need to modify :ada:`Logger` to check for the error message

      * No mechanism to retrieve input to a stub

.. container:: animate 2-

   * Test code inserted into :filename:`simple-test_data-tests.adb`

      .. code:: Ada 

         declare
            Result : Integer := 0;
            Status : Boolean;
         begin
            Sensor.Stub_Data.Set_Stub_Read_cac9ed_9101fc(Read_Result => 1234);
            Read (Sensor.Speed, Result, Status);
            AUnit.Assertions.Assert
              ( not Status,
               "Read negative failed - status");
         end;

   * Test code inserted into :filename:`logger.adb`

      * In :filename:`/gnattest_stub/stubs/default` folder

      .. code:: Ada 

         if Stub_Data_Log_Error_e35760_8432c2.Stub_Counter = 1 then
            AUnit.Assertions.Assert
              ( Message = "Invalid Speed",
               "Read negative failed - Log_Error");
         end if;

      * There are more advanced ways of ensuring stub is checked for appropriate text, but they're outside the scope of this class

----------------
Finish Testing
----------------

* Build as many more tests as you can in the remaining time

   * Experiment with both methods of setting return values

      * **Setter** subprogram
      * Edit stub directly

* Extra credit: figure out a better way of checking which test case called the stub

---------------------
Extra Credit Answer
---------------------

.. container:: latex_environment scriptsize

* :filename:`gnattest_stub/stubs/default/logger-stub_data.ads`

   .. code:: Ada

      type Caller_T is (Speed, Heading, Altitude, Unknown);
      type Stub_Data_Type_Log_Error_e35760_8432c2 is record
         Caller : Caller_T := Unknown;
         Stub_Counter : Natural := 0;
      end record;
      Stub_Data_Log_Error_e35760_8432c2 : Stub_Data_Type_Log_Error_e35760_8432c2;

* :filename:`gnattest_stub/stubs/default/logger.adb`

   .. code:: Ada

      Stub_Data_Log_Error_e35760_8432c2.Stub_Counter :=
            Stub_Data_Log_Error_e35760_8432c2.Stub_Counter + 1;
      if Stub_Data_Log_Error_e35760_8432c2.Caller = Speed then
         AUnit.Assertions.Assert
           ( Message = "Invalid Speed",
            "Read negative failed - Log_Error");
      end if;

* :filename:`simple-test_data-tests.adb`

   .. code:: Ada

      Sensor.Stub_Data.Set_Stub_Read_cac9ed_9101fc(Read_Result => 1234);
      Logger.Stub_Data.Stub_Data_Log_Error_e35760_8432c2.Caller :=
         Logger.Stub_Data.Speed;
