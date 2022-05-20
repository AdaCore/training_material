--------------
GNATtest Lab
--------------

* Use :toolname:`GNAT Studio` to open source project (:filename:`default.gpr`)

* Build :ada:`Main` to ensure everything is ready for testing

* Initialize test setup

  * :menu:`Analyze` |rightarrow| :menu:`GNATtest` |rightarrow| :menu:`Generate Unit Test Setup`

  * Enter values for *harness directory* and *tests directory*

  * Select **use stubbing** so each package is tested in isolation

    * Multiple harnesses will be built, one per unit
    * For each unit harness, calls to any other unit will be stubbed

  * Press **OK**

* Examine artifacts in *harness directory* and *tests directory*

----------------------------
Harness Directory Contents
----------------------------

* Only :filename:`gnattest_common.gpr` will remember modifications!

  * Common build settings for all test project files

* :filename:`test_drivers.gpr`

  * Aggregate project to build all test driver projects
  * Use :command:`gprbuild -P <harness-dir>/test_drivers.gpr` to build all tests

* :filename:`<unit>.Test_Data.Tests` folder

  * Test harness data for each unit

--------------------------
Tests Directory Contents
--------------------------

* Each package to be tested has a child unit (:ada:`Test_Data`) containing:

  * Record to store test data
  * Test Setup and Teardown procedures
  * Initially no data or processing
  * Modifications made here will be remembered
  * Note: If the package already has a :ada:`Test_Data` child, test generation will cause build problems

* Each test data unit has a child unit (:ada:`Tests`) containing

  * Definition and implementation of tests for each subprogram

* :ada:`Tests` specification is in :file:`<package_name>-test_data-tests.ads`

  * **Modifications made here will be overwritten!**
  * Subprogram name for testing has a unique identifier appended

    * To prevent conflicts with overloaded names

* :ada:`Tests` implementation is in :file:`<package_name>-test_data-tests.adb`

  * Implementation of tests for each subprogram
  * Bodies of tests are where you do your testing
  * Modifications made outside of *read only* blocks will be remembered

--------------------
Our First Test Run
--------------------

* Build all the test drivers

  :command:`gprbuild -P<harness-dir>/test_drivers.gpr`

* Run all tests

  :command:`gnattest <harness-dir>/test_drivers.list`

  * :filename:`test_drivers.list` is auto-generated. You can use your own file to limit tests being run

* All tests should fail with **Test not implemented** message

  * Mesage indicates where the failure was detected

----------------------------
Modify Our First Real Test
----------------------------

* We want to test :ada:`Inventory.Query`

  * Two tests:

    * Verify count is correct when queried item is in the database
    * Verify count is zero when queried item is not in the database

  * First test is simple: If database is empty, any query should return 0

    * Double-click *Query:test case* in *Tests* view

      .. image:: gnattest/inventory-query-testcase.jpg

-------------------------------
Implement Our First Real Test
-------------------------------

* Assert that when :ada:`Query` is called with a value not in inventory, the return value is 0

  * Modify :ada:`Test_Query` to verify the correct results, something like:

    .. code:: Ada

      AUnit.Assertions.Assert(Query("something") = 0,
                              "Query should have returned 0"); 

* Run the testcase

  * :menu:`Build` |rightarrow| :menu:`Run` |rightarrow| :menu:`Test Driver` |rightarrow| :menu:`inventory-test_data-tests-suite-test_runner`

  * Test now passes!

    * For "sanity", you could change :ada:`= 0` to fail the test and verify your error message

--------------------------------
Implement Our Second Real Test
--------------------------------

* Assert that when :ada:`Query` is called with a value in inventory, the return value is expected

  * We need to insert something into inventory

    * To do that we must call :ada:`Add`
    * Use constants for the input values to enforce consistency

  * Modify :ada:`Test_Query` to add a block of code for our second test

    .. code:: Ada

      declare
         Item   : constant String := "Rocket";
         Count  : constant        := 123;
         Result : Natural;
      begin
         Add (Item, Count);
         Result := Query (Item);
         Aunit.Assertions.Assert
           (Query (Item) = Count,
            "Query returned" & Result'image & " should have returned" &
            Count'image);
      end;

* Run the testcase

  * Test now passes!

    * For "sanity", you could change :ada:`= Count` to fail the test and verify your error message

-------------
Using Stubs
-------------

* Our code uses :ada:`Console` to centralize output. We want to capture data sent to :ada:`Console`

  * If we were doing this by hand, we would replace the body with our own version that printed the input values

* We want to test :ada:`Point_Of_Sale.Sell_Item` and verify the message it sends to the console

  * On examination, if we try to sell something we don't have, we get a message.
  * We'll test this scenario, because it doesn't require setup!

* But there is nothing in the harness call we can check!

  * We actually want to verify :ada:`Console.Print` got called correctly
  * We need to put our assertion in that routine

* Before we do this in the harness, we need to set up the stub package

-----------------------------------
Using Stubs - Setting Up The Stub
-----------------------------------

* In *Tests* view, double-click on **Console:stub body** to set up stub data for the :ada:`Print` subprogram

  * Note :ada:`Stub_Data_Print_0` is object where global data is stored
  * Right-click :ada:`Stub_Data_Print_0` to go to object declaration
  * We want to add a field to the record to store the message we expect

    .. code:: Ada

      type Stub_Data_Type_Print_0 is record
        Expected     : Unbounded_String :=
                       Null_Unbounded_String;
        Stub_Counter : Natural          := 0;
      end record;

    * Don't forget to add a with/use of :ada:`Ada.Strings.Unbounded_String`

  * We want to modify the *Set* routine to allow the caller to set data in this record

    .. code:: Ada

      procedure Set_Stub_Print_0 (Expected : String);

-------------------------------------
Using Stubs - Controlling Stub Data
-------------------------------------

* Right-click anywhere in the stub spec and select *Jump to Implementation File*

* Update the *Set* routine interface, and store the input paramter into the global object

  .. code:: Ada

    procedure Set_Stub_Print_0 (Expected : String) is
    begin
      Stub_Data_Print_0.Expected :=
          To_Unbounded_String (Expected);
    end Set_Stub_Print_0;

* Now the stub data is ready to verify input data

-------------------------------------
Using Stubs - Implementing The Stub
-------------------------------------

* In *Tests* view, double-click on **Console:stub body**

* We want to verify :ada:`S` parameter is what we expected

  * Need to compare :ada:`S` to :ada:`Expected` field of global data

  .. code:: Ada

    declare
      Expected : constant String :=
        To_String (Stub_Data_Print_0.Expected);
    begin
      Aunit.Assertions.Assert
        (Expected = S,
         "Expected:'" & Expected & "' got '" & S & "'");
    end;

  * Don't forget to add with/use for :ada:`Ada.Strings.Unbounded` and :ada:`Aunit.Assertions`

* Now the stub will verify its input value

-------------------------------------
Using Stubs - Running The Test
-------------------------------------

* In *Tests* view, double-click on **Sell_Item:test case** to set up test

* Uncomment and modify *Set* call to set message you expect

  * Try "Test" first to verify failure behavior

  .. code:: Ada

    Console.Stub_Data.Set_Stub_Print_0 ("Test");
    --  Inventory.Stub_Data.Set_Stub_Query_0( );
    --  Inventory.Stub_Data.Set_Stub_Remove_0( );

    Sell_Item ("something", 10);

* Run test

  * Should see

  ::

    point_of_sale.ads:5:4: error: corresponding test FAILED:
      Expected:'Test' got 'Not enough something in inventory'

* Update expected value and verify correct results

----------------
As Time Allows
----------------

* Repetition is the best teacher!

* See how many tests you can complete in the remaining time

