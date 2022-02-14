.. role:: ada(code)
   :language: ada

-----------------
Navigate the code
-----------------

1. Right click on :ada:`Time_Step`
2. Click "Goto Spec" and "Go Full Spec" to navigate the program source

.. image:: gnat_studio/ui/menu_goto_full_declaration_1.png

3. See how it brings you to the procedure

.. image:: gnat_studio/ui/menu_goto_full_declaration_2.png

“Goto Declaration” is probably the most useful :toolname:`GNAT Studio` feature for navigating and
understanding large programs.


*Extra credit*: How does the :toolname:`GNAT Studio` editor know where to find the declaration of an
entity like :ada:`Tracked`?

--------------
Debug the code
--------------

You can use :toolname:`GNAT Studio` to debug your program, by clicking the “Build and Debug” action.

Let’s do a quick debugging session on the main program.

1. Click on the Build and Debug icon

.. image:: gnat_studio/ui/debug_button.png

2. :toolname:`GNAT Studio` will go into debugging perspective, which you can see because a few
   more views have popped up.

.. image:: gnat_studio/ui/debug_perspective.png

Eight new buttons are added to the toolbar, allowing for precise debugging actions.

.. image:: gnat_studio/ui/debug_buttons_labels.png

3. Open the main source file, :file:`main.adb`
4. Click on the second :ada:`Time_Step` call, around line number "50",
   then click on the “Debug Continue” icon in the main toolbar.

.. image:: gnat_studio/debug_project/1.png

the program will stop at the first executable line.

Click “Debug Continue” again and program execution should then stop at the :ada:`Time_Step` call in
the main file.

.. image:: gnat_studio/debug_project/3.png

5. Click on the “Debug Next” icon

See the program execution going to the next line. Do it again to reach line 62 –
the next call to :ada:`Time_Step`.

6. Click on the “Debug Step” icon to step into the :ada:`Time_Step` subprogram. The file
   :file:`radar_internals.adb` should then open.

7. Click on the “Debug Finish” icon to step out of the subprogram.
8. Let’s spy the value of a local variable! Right click on the :ada:`John_Connor` identifier, and go 
   into Debug -> Display :ada:`John_Connor` in variables view menu.

.. image:: gnat_studio/debug_project/4.png

:ada:`John_Connor` will be shown in the “Debugger Variables” view.

.. image:: gnat_studio/ui/debug_variables_view.png
    :height: 300px
