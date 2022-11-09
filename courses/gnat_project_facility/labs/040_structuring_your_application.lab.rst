:title: Structuring Your Application Lab 

* Source is included in folder :filename:`040_structuring_your_application`

* **Very** simplistic speed monitor

  * Reads current distance
  * Determines amount of time since last read
  * Calculates speed
  * Sends message

* Four subsystems

  * :filename:`Base` - types and speed calculator
  * :filename:`Sensors` - reads distance from some register
  * :filename:`Messages` - sends message to some memory location
  * :filename:`Application` - main program

* We could build one GPR file and point to all source directories

  * But as our application grew, this would become harder to maintain

---------------------
Assignment Part One
---------------------

1. Build GPR files for each subsystem

  * Hint: These subsystems *depend* on each other, they do not *override* source files

  * As you build each GPR file, run :command:`gprbuild -P <gprfile>` to make sure everything works

  * Main program is in :filename:`main.adb`

2. Run :ada:`main`

  * This will fail (leading up to Part Two of the assignment)

3. Modify :filename:`base_types.ads`

  * Just so source code needs to be compiled

4. Rebuild your main program

  * Even though the modified source file is not directly referenced in the main GPR file, :toolname:`gprbuild` should compile everything it needs

--------------------------------
Assignment Part One - Solution
--------------------------------

.. code:: Ada

   with "../base/base.gpr";
   with "../messages/messages.gpr";
   with "../sensors/sensors.gpr";
   project Application is
      for Source_Dirs use ("src");
      for Object_Dir use "obj";
      for Main use ("main.adb") & project'Main;
   end Application;

   with "../base/base.gpr";
   project Messages is
       for Source_Dirs use ("src");
       for Object_Dir use "obj";
   end Messages;

   with "../base/base.gpr";
   project Sensors is
       for Source_Dirs use ("src");
       for Object_Dir use "obj";
   end Sensors;


   project Base is
       for Source_Dirs use ("src");
       for Object_Dir use "obj";
   end Base;

---------------------
Assignment Part Two
---------------------

1. Build GPR files to create test stubs for :ada:`Odometer` and :ada:`Sender`

  * Test bodies exist in the appropriate :filename:`test` subfolders

  * Create extensions for :filename:`messages.gpr` and :filename:`sensors.gpr`

    * We want to inherit the package spec, but use the "test" package bodies

2. Build a GPR file for the main application

  * :ada:`Main` still works, we just need the GPR file to access our stubs
  * We could create a new GPR file, or extend the original. Which is easier?

3. Build and run your main program

--------------------------------
Assignment Part Two - Solution
--------------------------------

* :filename:`messages/test` directory

   .. code:: Ada

      project Messages_Test extends "../Messages.gpr" is
         for Source_Dirs use (".");
      end Messages_Test;

* :filename:`sensors/test` directory

   .. code:: Ada

      project Sensors_Test extends "../sensors.gpr" is
         for Source_Dirs use (".");
      end Sensors_Test;

* :filename:`test` directory

   .. code:: Ada

      with "../messages/test/messages_test.gpr";
      with "../sensors/test/sensors_test.gpr";
      project Test extends "../application/application.gpr" is
         for Main use ("main.adb") & project'Main;
      end Test;
