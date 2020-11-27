.. include:: support_files/docs_common.rst

---------------
Getting Started
---------------

1. Start GNAT Studio
2. Select "Open Project"

.. image:: CE_Open_Labs_1.png
    :height: 200px

3. Select the project directory
4. Open the project :code:`radar.gpr` file

.. image:: CE_Open_Labs_2.png
    :height: 300px

5. On the left, open the :code:`main.adb` file under the :code:`src/` directory

.. image:: CE_Open_Labs_3.png
    :height: 300px

6. Locate and click on the "Compile & Run" button in GNAT Studio.

.. image:: GS_Compile_And_Run_Button.png

Notice the compilation messages and run execution status at
the bottom of screen.

It should fail with an error about missing statement around :ada:`end Main;`

.. image:: Lab_1_Compile_Fail.png
    :height: 300px

You can clean the project files by click the "Clean" button

.. image:: GS_Clean_Button.png
