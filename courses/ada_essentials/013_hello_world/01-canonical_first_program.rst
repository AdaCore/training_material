=================
Introducing Ada
=================

-------------------------
Canonical First Program
-------------------------

"Hello World" - written in Ada!

.. container:: overlay 1

   .. image:: hello_world_0.svg

.. container:: overlay 2

   .. image:: hello_world_1.svg

.. container:: overlay 3

   .. image:: hello_world_2.svg

.. container:: overlay 4

   .. image:: hello_world_3.svg

.. container:: overlay 5

   .. image:: hello_world_4.svg

.. container:: overlay 6

   .. image:: hello_world_5.svg

.. container:: overlay 7

   .. image:: hello_world_6.svg

.. container:: animate 2-

   * :ada:`with` - package dependency (similar to ``import`` or ``#include``)

.. container:: animate 3-

   * :ada:`--` - Comment (always goes to end of line)

.. container:: animate 4-

   * :ada:`procedure` - subprogram declaration (name of subprogram is ``Hello_World``)

.. container:: animate 5-

   * :ada:`begin` - used to start a block of statements

.. container:: animate 6-

   * :ada:`Ada.Text_IO.Put_Line` is a subprogram that prints a string (it's defined
     in the package we specified on line 1)

.. container:: animate 7-

   * :ada:`end` - used to end a block of statements. It's optional to add the
     name of the block you are ending
