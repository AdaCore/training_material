-------------------------------
GNATcoverage From GNAT Studio
-------------------------------

* We are going to get 100% Statement Coverage on the example from the module

   * But now we're doing it from the IDE!

* Copy the :filename:`cover_040_gnatstudio` lab from the course materials location

* Contents of the folder:

  * :filename:`default.gpr` - project file
  * :filename:`src` - source directory

*Note: Many of the following pages use animation to first give you a task and then show you how to do it.* :menu:`Page Down` *does not always go to the next page!*

-------------------
Start GNAT Studio
-------------------

* Start :toolname:`GNAT Studio` and open project :filename:`default.gpr`

   * From the **Start Menu** or **Application Launcher**

      * Then :menu:`File` |rightarrow| :menu:`Open Project` and navigate to the file

   * From a command prompt

      * :command:`gnatstudio -P /path/to/default.gpr` **OR**
      * :command:`gnatstudio` if :filename:`default.gpr` is the only project in the current directory

-------------------------
Instrument Your Project
-------------------------

* Always best to make sure your code compiles first

   * :menu:`Build` |rightarrow| :menu:`Project` |rightarrow| :menu:`Build All` **OR**
   * *Build target Build All* icon

* Set the coverage type to **Stmt**

.. container:: animate 2-

   :menu:`Edit` |rightarrow| :menu:`Project Properties` |rightarrow| :menu:`Coverage`

   * Instrument the project

.. container:: animate 3-

   :menu:`Analyze` |rightarrow| :menu:`Coverage` |rightarrow| :menu:`GNATcoverage Source Traces` |rightarrow| :menu:`Run All Actions` **OR**

   Run GNATCoverage with instrumentation test_driver.adb* icon

      * Your coverage report should be displayed

--------------------------------
Navigating the Coverage Report
--------------------------------

* Experiment with the coverage report

   * Click on column titles to change order
   * Click expansion triangle to see coverage per subprogram
   * Double-click on an entity to see annotated coverage

* Next, edit the test driver to test more subprograms and run the new driver

.. container:: animate 2-

   * If you clicked your normal *Build & Run test_driver.adb* icon, coverage didn't update!

      * You need to rebuild the coverage to get the updated code


