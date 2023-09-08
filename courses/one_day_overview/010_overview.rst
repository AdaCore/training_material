**********
Overview
**********

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

===================
About This Course
===================

--------
Styles
--------

* :dfn:`This` is a definition
* :filename:`this/is/a.path`
* :ada:`code is highlighted`
* :command:`commands are emphasised --like-this`

==============
Introduction
==============

--------------
Introduction
--------------

* This is a 1 day crash course on Ada
* Full content goes over 2 weeks
* Full content on `github <https://github.com/AdaCore/training_material/tree/master/courses/fundamentals_of_ada>`_
* See also `Learn.adacore.com <https://learn.adacore.com/>`_

-------------------------
*Core* Language Content
-------------------------

* Ada is a **compiled**, **multi-paradigm** language

   - Exceptions
   - Generic units
   - Dynamic memory management
   - Low-level programming
   - Object-Oriented Programming (OOP)
   - Concurrent programming
   - Contract-Based Programming

* With a **static** and **strong** type model

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
