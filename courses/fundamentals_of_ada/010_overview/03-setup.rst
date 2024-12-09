=======
Setup
=======

-------------------------
Canonical First Program
-------------------------

.. code:: Ada

   1 with Ada.Text_IO;
   2 -- Everyone's first program
   3 procedure Say_Hello is
   4 begin
   5   Ada.Text_IO.Put_Line ("Hello, World!");
   6 end Say_Hello;

* Line 1 - :ada:`with`  - Package dependency
* Line 2 - :ada:`--` - Comment
* Line 3 - :ada:`Say_Hello` - Subprogram name
* Line 4 - :ada:`begin` - Begin executable code
* Line 5 - :ada:`Ada.Text_IO.Put_Line ()` - Subprogram call
* (cont) - :ada:`"Hello, World!"` - String literal (type-checked)

----------------------------------
"Hello World" Lab - Command Line
----------------------------------

* Use an editor to enter the program shown on the previous slide

   - Use your favorite editor or just gedit/notepad/etc.

* Save and name the file :filename:`say_hello.adb` exactly

   - In a command prompt shell, go to where the new file is located and issue the following command:

      + :command:`gprbuild say_hello`

* In the same shell, invoke the resulting executable:

   - :command:`say_hello` (Windows)
   - :command:`./say_hello` (Linux/Unix)

---------------------------------------------
"Hello World" Lab - :toolname:`GNAT Studio`
---------------------------------------------

* Start :toolname:`GNAT Studio` from the command-line (:command:`gnatstudio`) or Start Menu
* :menu:`Create new project`

   - Select :menu:`Simple Ada Project` and click :menu:`Next`
   - Fill in a location to to deploy the project
   - Set **main name** to *say_hello* and click :menu:`Apply`

* Expand the **src** level in the Project View and double-click :filename:`say_hello.adb`

   - Replace the code in the file with the program shown on the previous slide

* Execute the program by selecting :menu:`Build` :math:`\rightarrow` :menu:`Project` :math:`\rightarrow` :menu:`Build & Run` :math:`\rightarrow` :menu:`say_hello.adb`

   - Shortcut is the :math:`\blacktriangleright` in the icons bar

* Result should appear in the bottom pane labeled *Run: say_hello.exe*

--------------------------------------
Note on GNAT File Naming Conventions
--------------------------------------

* GNAT compiler assumes one compilable entity per file

  * Package specification, subprogram body, etc
  * So the body for :ada:`say_hello` should be the only thing in the file

* Filenames should match the name of the compilable entity

  * Replacing "." with "-"
  * File extension is ".ads" for specifications and ".adb" for bodies
  * So the body for :ada:`say_hello` will be in :filename:`say_hello.adb`

    * If there was a specification for the subprogram, it would be in :filename:`say_hello.ads`

* This is the **default** behavior. There are ways around both of these rules

  * For further information, see Section 3.3 *File Naming Topics and Utilities* in the **GNAT User's Guide**
