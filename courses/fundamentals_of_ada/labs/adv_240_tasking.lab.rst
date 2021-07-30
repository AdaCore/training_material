
-------------
Tasking Lab
-------------

(Simplified) Graphical Display System

  * Overview

    * User should be able to place graphical objects on the screen
    * Objects should be updated regularly, either by user or algorithm

  * Goals

    * Create a GUI that allows you to

      * Create new objects
      * Modify attributes of existing objects

**NOTE** This is not a simple project (which is why it's the last one)!

  * We have the rest of the day to work on it
  * We will pause periodically to make sure everyone is progressing

----------------------
Project Requirements
----------------------

* Tasks / Protected Objects

  * Use tasking to ensure every object updates at its own rate
  * Use protected objects to make sure objects aren't redrawn while being modified

* Graphical Interface

  * Unzip :filename:`graphical_interface` ZIP file

    * Open a command prompt and run :filename:`install.bat`
    * Run :toolname:`GNAT Studio` from this command prompt

  * When you create your project, you need to modify your GPR file to point to graphics

    * Right-click on your project name in the project view
    * Select :menu:`Project` :math:`\rightarrow` :menu:`Edit source file` to edit the GPR file
    * Add :ada:`with "game_support.gpr";` to the top of the file

---------------------------
Graphical Interface Hints
---------------------------

* Units :ada:`Display` and :ada:`Display.Basics` should be all you need to worry about

  * :ada:`Display` defines the colors used
  * :ada:`Display.Basics` contains the graphical primitives

    * **New_** subprograms create objects
    * **Set_** and **Get_** subprogram allow modification/query

----------------
Plan of Attack
----------------

* Start **SIMPLE!**

  1. Make sure you can draw an object!
  2. Make sure you can modify an object

* Add tasking

  1. Create a task that will regularly modify an object

    * Change location, alternate color, etc

  2. Create a collection of tasks so you can have one per object

* Add protected types

  1. Objects should be protected so you cannot modify one while it is being drawn
  2. Use the protected types to modify/query objects

* Extra Credit

  * Add user controls for object creation / modification

-------------------------------
Tasking Lab Solution - Simple
-------------------------------

* :filename:`default.gpr`

  .. code:: Ada

    with "game_support.gpr";
    project Default is
      for Main use ("main.adb");
    end Default;

* :filename:`main.adb`

  .. code:: Ada

    with Display;
    with Display.Basic;
    procedure Main is
       Id : Display.Basic.Shape_Id;
    begin
       Id := Display.Basic.New_Torus
           (X            => 1.0,
            Y            => 1.0,
            Inner_Radius => 10.0,
            Outer_Radius => 20.0,
            Color        => Display.Red);
    end Main;
  
--------------------------------------
Tasking Lab Solution - Shapes (Spec)
--------------------------------------

.. container:: source_include labs/answers/adv_240_tasking.txt :start-after:--Shapes_Spec :end-before:--Shapes_Spec :code:Ada

--------------------------------------
Tasking Lab Solution - Monitor (Spec)
--------------------------------------

.. container:: source_include labs/answers/adv_240_tasking.txt :start-after:--Monitor_Spec :end-before:--Monitor_Spec :code:Ada

-----------------------------
Tasking Lab Solution - Main
-----------------------------

.. container:: source_include labs/answers/adv_240_tasking.txt :start-after:--Main :end-before:--Main :code:Ada

-----------------------------------------------------
Tasking Lab Solution - Shapes Protected Type (Spec)
-----------------------------------------------------

.. container:: source_include labs/answers/adv_240_tasking.txt :start-after:--Shape_Data_T_Spec :end-before:--Shape_Data_T_Spec :code:Ada

------------------------------------------------------
Tasking Lab Solution - Shapes (Creation Subprograms)
------------------------------------------------------

.. container:: source_include labs/answers/adv_240_tasking.txt :start-after:--Shapes_Creation :end-before:--Shapes_Creation :code:Ada

-------------------------------------------------------------
Tasking Lab Solution - Shapes (Interface to Protected Type)
-------------------------------------------------------------

.. container:: source_include labs/answers/adv_240_tasking.txt :start-after:--Shapes_Interface :end-before:--Shapes_Interface :code:Ada

-----------------------------------------------------
Tasking Lab Solution - Shapes Protected Type (Body)
-----------------------------------------------------

.. container:: source_include labs/answers/adv_240_tasking.txt :start-after:--Shape_Data_T_Body :end-before:--Shape_Data_T_Body :code:Ada

---------------------------------------
Tasking Lab Solution - Monitor (Body)
---------------------------------------

.. container:: source_include labs/answers/adv_240_tasking.txt :start-after:--Monitor_Body :end-before:--Monitor_Body :code:Ada

