:title: Getting started


The purpose of this exercise is to discover the basics of the GNAT toolchain on
Windows.

==========
Question 1
==========

We will firstly build a simple project using the command line. The project file is
at the root of the tutorial directory, and it is called :file:`simple.gpr`.
You can build it with the command:

:code:`gprbuild –Psimple`

This will compile all the sources of the project, as well as the support libraries
needed to execute your source code on Windows. 
Then run the command:

:code:`obj\main`

Note that, this time, the project file specifies the sub-directory :file:`obj` to hold
all object code and binaries.
When this is done you can clean up your workspace of the executable and object files,
by running:

:code:`gprclean –Psimple`

==========
Question 2
==========

During the training, we will use GNAT Studio, AdaCore’s integrated development
environment. You can run it to open the simple project via:

:code:`gnatstudio –Psimple`

After that, you can use the “Build and Run” action, that is accessible via the play
icon in the main toolbar, to automate what we did in part 1. It will rebuild
everything necessary and run the resulting program.

==========
Question 3
==========

Depending on your computer’s screen and its resolution, the Windows and Ball might be
a bit small.
Adjust the :code:`Width`, :code:`Height` and :code:`Ball_Radius` constants to make
the Window and Ball size more suitable for your computer.

Alter the colour of the Ball.
Hint: Right-click on the literal “Red” in the call to 
:code:`Draw_Sphere`, and select “Goto Declaration”. This will show you the
specification of the package where Red (and other colours) are declared.

==========
Question 4
==========
Right click on other entities in the :code:`Main` program (types, variables,
subprograms etc.) and use
“Goto Declaration” and “Goto Body” to navigate the program source.
“Goto Declaration” is probably the most useful GNAT Studio feature for navigating and
understanding large programs.

*Extra credit*: How does the GNAT Studio editor know where to find the declaration of
an entity like “Red”?

==========
Question 5
==========

You can use GNAT Studio to debug your program, by clicking the “Build and Debug”
action. Let’s do a quick debugging session on the main program.

1. Click on the Build and Debug icon
2. GNAT Studio will go into debugging perspective, which you can see because a few
   more views have popped up.
3. Open the main source file, :file:`main.adb`
4. Click on the line number “63”, and the click on the “Debug Continue” icon in the
   main toolbar. The program will stop at the first executable line.
   Click “Debug Continue” again and program execution should then stop at line 63 of
   the main file.
5. Click on the “debug next” icon and see the program execution going to the next
   line. Do it again to reach line 65 – the call to :code:`Draw_Sphere`.
6. Click on the “debug step” icon to step into the :code:`Draw_Sphere` subprogram.
   The file :file:`display-basic.adb` should then open.
7. Click on the “debug finish” icon to step out of the subprogram.
8. You can then repeatedly press the “debug continue” button, and see the ball move
   slowly on your board’s screen.

