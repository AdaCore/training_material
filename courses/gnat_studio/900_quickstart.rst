.. role:: ada(code)
   :language: ada

==================================
Getting Started with GNAT Studio
==================================

----------------------
1. Start GNAT Studio
----------------------

Start from the command line using :command:`gnatstudio` or from the application menu via :toolname:`GNAT Studio`

   *Note: If you use the command line, and there is only one GPR file in the directory where you start GNAT Studio, then the tool will open that project automatically (so you can skip to Step 3)*

-----------------------------
2. Open an Existing Project
-----------------------------

.. columns::

   .. column::

      a. From the *Welcome* dialog, select :menu:`Open Project`

         .. image:: ../../images/gnat_studio/quickstart/getting_started-welcome_dialog.jpg

   .. column::

      b. Select the project directory
      c. Open the project :filename:`radar.gpr` file

         .. image:: ../../images/gnat_studio/quickstart/getting_started-open_project.jpg

-----------------------
3. Open a Source File
-----------------------

In the *Project* pane on the left, double-click :filename:`main.adb` file under the :filename:`src/` directory

   .. image:: ../../images/gnat_studio/quickstart/getting_started-main.jpg

-------------------------
4. Build the Executable
-------------------------

.. columns::

   .. column::

      a. Locate and click on the :menu:`Compile & Run` button

         .. image:: ../../images/gnat_studio/run_main_icon.jpg

   .. column::

      b. Notice the compilation messages and run execution status at the bottom of screen.

         *(It should succeed)*

         .. image:: ../../images/gnat_studio/quickstart/getting_started-success.jpg

-------------------------
5. Remove Build Results
-------------------------

 You can clean the project files by click the :menu:`Clean` button

   .. image:: ../../images/gnat_studio/clean_icon.jpg
