========
Lab
========

-----------------
Hello World Lab
-----------------

* This lab focuses more on running labs and verifying your setup

   * Goal for this lab is to write an application that says :command:`Hello, World`

* Almost all our labs are the same. In each lab, we will

   * Copy the appropriate :filename:`prompt` directory
   * Open the project that you just copied over
   * Make the changes necessary to perform the lab
   * Compile and run the lab
   * Compare the actual results to the expected results
   * Refer to the :filename:`answer` directory if you need help

* This particular lab will walk you through those steps

--------------------------
Step 1 - Copy the Prompt
--------------------------

* You should have downloaded (or received) a :filename:`labs`
  folder

   * This folder contains a directory for each module in the
     class that has a lab

* In each module directory, you should find a :filename:`prompt`
  and :filename:`answer` folder

* The :filename:`prompt` folder contains

   * Project file (:filename:`default.gpr`) that instructs
     the compilation system on how to build the application
   * Ada file(s) containing the source code for the application

* Create a folder for this class. As you do each lab, copy
  the module directory into the class folder

---------------------------
Step 2 - Open the Project
---------------------------

Using :toolname:`GNAT Studio`

   * If :toolname:`GNAT Studio` is on your path, you can either

      * Double-click the :filename:`default.gpr` project file
      * Run :command:`gnatstudio default.gpr` file from a command prompt in the correct folder

   * Open the :toolname:`GNAT Studio` application

      * :menu:`File` |rightarrow| :menu:`Open Project` to open the project file

-------------------------------------
Step 3 - Make the Necessary Changes
-------------------------------------

* In the left pane (**Project** tab if it not selected), expand the triangles until you
  see :filename:`hello_world.adb` - double-click it

* In our example, we want to follow the "prompt" comment on line 5

   * On line 6, replace ``<something>`` with ``Hello, World``

----------------------------------
Step 4 - Compile and Run the Lab
----------------------------------

.. image:: compile_and_build_buttons.jpg

   *(also available from* :menu:`Build` *menu)*

* After execution, search for the :menu:`Run` tab on the **Messages** window

-------------------------------------
Step 5 - Compare Actual to Expected
-------------------------------------

* If the actual results match the goal of the lab

   * Congratulations - you've done it!
   * In this lab, we should see :command:`Hello, World` in the :menu:`Run` tab

* If they don't, go back to Step 3

   * Or go on to Step 6

--------------------------------
Step 6 - Looking at the Answer
--------------------------------

* In the :filename:`answer` folder will be the source code for **a** correct solution

   * Look at the part you think is most likely wrong

      * Then go to Step 3 and see if that hint helps

   * Continue until you get the expected result

      * Even if that means copying the whole answer so you can understand it

* Even when you got it right yourself, looking at the answer may give you another
  method of solving the problem
