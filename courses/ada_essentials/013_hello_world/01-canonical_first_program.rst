=================
Introducing Ada
=================

-------------------------
Canonical First Program
-------------------------

Everyone's first program - written in Ada

.. container:: overlay 1

   | 1 :color-white:`-`    ``with Ada.Text_IO;``
   | 2 :color-white:`-`    ``-- Everyone's first program``
   | 3 :color-white:`-`    ``procedure Hello_World is``
   | 4 :color-white:`-`    ``begin``
   | 5 :color-white:`----` ``Ada.Text_IO.Put_Line ("Hello, World!");``
   | 6 :color-white:`-`    ``end Hello_World;``

.. container:: overlay 2

   | 1 :color-white:`-`    :color-red:`with Ada.Text_IO;`
   | 2 :color-white:`-`    ``-- Everyone's first program``
   | 3 :color-white:`-`    ``procedure Hello_World is``
   | 4 :color-white:`-`    ``begin``
   | 5 :color-white:`----` ``Ada.Text_IO.Put_Line ("Hello, World!");``
   | 6 :color-white:`-`    ``end Hello_World;``

.. container:: overlay 3

   | 1 :color-white:`-`    ``with Ada.Text_IO;``
   | 2 :color-white:`-`    :color-red:`-- Everyone's first program`
   | 3 :color-white:`-`    ``procedure Hello_World is``
   | 4 :color-white:`-`    ``begin``
   | 5 :color-white:`----` ``Ada.Text_IO.Put_Line ("Hello, World!");``
   | 6 :color-white:`-`    ``end Hello_World;``

.. container:: overlay 4

   | 1 :color-white:`-`    ``with Ada.Text_IO;``
   | 2 :color-white:`-`    ``-- Everyone's first program``
   | 3 :color-white:`-`    :color-red:`procedure Hello_World is`
   | 4 :color-white:`-`    ``begin``
   | 5 :color-white:`----` ``Ada.Text_IO.Put_Line ("Hello, World!");``
   | 6 :color-white:`-`    ``end Hello_World;``

.. container:: overlay 5

   | 1 :color-white:`-`    ``with Ada.Text_IO;``
   | 2 :color-white:`-`    ``-- Everyone's first program``
   | 3 :color-white:`-`    ``procedure Hello_World is``
   | 4 :color-white:`-`    :color-red:`begin`
   | 5 :color-white:`----` ``Ada.Text_IO.Put_Line ("Hello, World!");``
   | 6 :color-white:`-`    ``end Hello_World;``

.. container:: overlay 6

   | 1 :color-white:`-`    ``with Ada.Text_IO;``
   | 2 :color-white:`-`    ``-- Everyone's first program``
   | 3 :color-white:`-`    ``procedure Hello_World is``
   | 4 :color-white:`-`    ``begin``
   | 5 :color-white:`----` :color-red:`Ada.Text_IO.Put_Line ("Hello, World!");`
   | 6 :color-white:`-`    ``end Hello_World;``

.. container:: overlay 7

   | 1 :color-white:`-`    ``with Ada.Text_IO;``
   | 2 :color-white:`-`    ``-- Everyone's first program``
   | 3 :color-white:`-`    ``procedure Hello_World is``
   | 4 :color-white:`-`    ``begin``
   | 5 :color-white:`----` ``Ada.Text_IO.Put_Line ("Hello, World!");``
   | 6 :color-white:`-`    :color-red:`end Hello_World;`

.. container:: animate 2-

   * :ada:`with` - package dependency (similar to ``import`` or ``#include``)

.. container:: animate 3-

   * :ada:`--` - Comment (always goes to end of line)

.. container:: animate 4-

   * :ada:`procedure` - subprogram declaration (name of subprogram is ``Hello_World``)

.. container:: animate 5-

   * ``begin`` - used to start a block of statements

.. container:: animate 6-

   * ``Ada.Text_IO.Put_Line`` is a subprogram that prints a string (it's defined
     in the package we specified on line 1)

.. container:: animate 7-

   * ``end`` - used to end a block of statements. It's optional to add the
     name of the block you are ending.

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
