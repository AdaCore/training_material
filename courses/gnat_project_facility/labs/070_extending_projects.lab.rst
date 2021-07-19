------------------------
Extending Projects Lab
------------------------

Premise: A simplistic application to read data from a memory location

* Go to the :filename:`extending_projects` directory (under :filename:`materials`)

* Create a project file to build the "original" application (source files in :filename:`original`)

   * Build and run application (main program :filename:`reader.adb`)
   * Application fails because in our environment, memory location is invalid

We want a *test_driver* application that uses the correct interface, but replaces the :ada:`Sensors` package body with a mockup

* Create a project file to build the a "test" application (source files in :filename:`test`)

   * Project file should use information from original project file

      * But adds the new main program and mockup

   * Build and run application (main program :filename:`test_driver.adb`)
   * Application will now run because the :ada:`Sensors` body is a mockup

